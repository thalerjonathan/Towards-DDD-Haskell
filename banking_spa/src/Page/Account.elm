module Page.Account exposing (Model, Msg, init, subscriptions, update, view)

import Http
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Either as E

import Api
import Page.Utils as Utils

type alias Iban = String
type alias CustomerId = String
type alias CustomerName = String

type alias LoadedState =
  { customerId     : CustomerId
  , customerName   : CustomerName
  , depositAmount  : Float
  , withdrawAmount : Float
  , transferAmount : Float
  , transferIban   : String
  , transferRef    : String
  , account        : Api.Account
  }

type Model 
  = Loading Iban CustomerId CustomerName
  | Failure Iban String
  | Loaded LoadedState
  
type Msg
  = AccountLoaded (Result Http.Error Api.Account)
  | DepositAmountUpdated String
  | Deposit
  | DepositReply (Result Http.Error Api.TxCommandResponse)
  | WithdrawAmountUpdated String
  | Withdraw
  | WithdrawReply (Result Http.Error Api.TxCommandResponse)
  | TransferIbanUpdated String
  | TransferAmountUpdated String
  | TransferReferenceUpdated String
  | Transfer
  | TransferReply (Result Http.Error Api.TxCommandResponse)

init : Iban -> CustomerId -> CustomerName -> (Model, Cmd Msg)
init iban cid cname = 
  ( Loading iban cid cname
  , Http.get
      { url = "/rest/account/" ++ iban
      , expect = Http.expectJson AccountLoaded Api.accountDecoder
      }
  )

mkInitLoadedState : Api.Account -> CustomerId -> CustomerName -> LoadedState
mkInitLoadedState a cid cname =
  { customerId = cid
  , customerName = cname
  , depositAmount = 0
  , withdrawAmount = 0
  , transferAmount = 0
  , transferIban = ""
  , transferRef = ""
  , account = a
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model) of
    (AccountLoaded result, Loading iban cid cname) ->
      case result of
        Ok a ->
          (Loaded (mkInitLoadedState a cid cname), Cmd.none)

        Err err ->
          (Failure iban (Utils.errorToString err), Cmd.none)

    (DepositAmountUpdated str, Loaded s) ->
      (Loaded { s | depositAmount = (Maybe.withDefault 0 (String.toFloat str))}, Cmd.none)

    (Deposit, Loaded s) ->
      (Loaded { s | depositAmount = 0}, 
        Http.post
        { url = "/rest/account/" ++ s.account.details.iban ++ "/deposit/" ++ (String.fromFloat s.depositAmount)
        , body = Http.emptyBody
        , expect = Http.expectJson DepositReply Api.commandResponseDecoder
        }
      )

    (DepositReply result, Loaded s) ->
      case result of
        Ok resp ->
          case resp of
            (E.Left err) ->
              (Failure s.account.details.iban err, Cmd.none)
            
            (E.Right tx) ->
              let 
                accUpd = updateAccountWithNewTX s.account tx
              in 
                (Loaded { s | account = accUpd }, Cmd.none)

        Err err ->
          (Failure s.account.details.iban (Utils.errorToString err), Cmd.none)

    (WithdrawAmountUpdated str, Loaded s) ->
      (Loaded { s | withdrawAmount = (Maybe.withDefault 0 (String.toFloat str))}, Cmd.none)

    (Withdraw, Loaded s) ->
      (Loaded { s | withdrawAmount = 0 }, 
        Http.post
        { url = "/rest/account/" ++ s.account.details.iban ++ "/withdraw/" ++ (String.fromFloat s.withdrawAmount)
        , body = Http.emptyBody
        , expect = Http.expectJson WithdrawReply Api.commandResponseDecoder
        }
      )

    (WithdrawReply result, Loaded s) ->
      case result of
        Ok resp ->
          case resp of
            (E.Left err) ->
              (Failure s.account.details.iban err, Cmd.none)
            
            (E.Right tx) ->
              let 
                accUpd = updateAccountWithNewTX s.account tx
              in 
                (Loaded { s | account = accUpd }, Cmd.none)

        Err err ->
          (Failure s.account.details.iban (Utils.errorToString err), Cmd.none)

    (TransferAmountUpdated str, Loaded s) ->
      (Loaded { s | transferAmount = (Maybe.withDefault 0 (String.toFloat str))}, Cmd.none)

    (TransferIbanUpdated str, Loaded s) ->
      (Loaded { s | transferIban = str}, Cmd.none)

    (TransferReferenceUpdated str, Loaded s) ->
      (Loaded { s | transferRef = str}, Cmd.none)

    (Transfer, Loaded s) ->
      (Loaded { s | transferAmount = 0, transferIban = "", transferRef = "" }, 
        Http.post
        { url = "/rest/account/" ++ s.account.details.iban ++ "/transfer/" ++ s.transferIban ++ "/" ++ (String.fromFloat s.transferAmount) ++ "/" ++ s.transferRef
        , body = Http.emptyBody
        , expect = Http.expectJson TransferReply Api.commandResponseDecoder
        }
      )

    (TransferReply result, Loaded s) ->
      case result of
        Ok resp ->
          case resp of
            (E.Left err) ->
              (Failure s.account.details.iban err, Cmd.none)
            
            (E.Right tx) ->
              let 
                accUpd = updateAccountWithNewTX s.account tx
              in 
                (Loaded { s | account = accUpd }, Cmd.none)

        Err err ->
          (Failure s.account.details.iban (Utils.errorToString err), Cmd.none)

    (_, _) ->
      (model, Cmd.none)

view : Model -> List (Html Msg)
view model =
  case model of 
    Loading iban _ _ -> 
      [ H.h2 [] [ H.text ("Loading account " ++ iban ++ "...") ]]
    
    Failure iban err -> 
      [ H.h2 [] [ H.text ("Error in account " ++ iban ++ ": '" ++ err ++ "'") ]]
    
    Loaded s ->
      [ H.h1 [] [ H.text s.customerName ]
      , H.h2 [] [ H.text (s.account.details.iban ++ " (" ++ s.account.details.accountType ++ ")") ]
      , H.h3 [] [ H.text ("Balance: " ++ (String.fromFloat s.account.details.balance)) ]

      , H.a [ A.class "btn btn-outline-primary", A.href ("/spa/customer/" ++ s.customerId) ] [ H.text "Back"]

      , H.hr [] []

      -- deposit form
      , H.div [ A.class "container-fluid mt-1 p-2 bg-light text-dark rounded-3" ]
        [ H.input [ E.onInput DepositAmountUpdated, A.type_ "number", A.step "0.01", A.value (String.fromFloat s.depositAmount) ] [] 
        , H.button [ E.onClick Deposit, A.class "btn btn-primary" ] [ H.text "Deposit" ]
        ]

      -- withdaw form
      , H.div [ A.class "container-fluid mt-1 p-2 bg-light text-dark rounded-3" ]
        [ H.input [ E.onInput WithdrawAmountUpdated, A.type_ "number", A.step "0.01", A.value (String.fromFloat s.withdrawAmount) ] []
        , H.button [ E.onClick Withdraw, A.class "btn btn-primary" ] [ H.text "Withdraw" ]
        ]

      -- transfer form
      , H.div [ A.class "container-fluid mt-3 p-2 bg-light text-dark rounded-3" ]
        [ H.div [ A.class "input-group input-group-sm mb-3" ]
            [ H.span [ A.class "input-group-text" ] 
              [ H.text "Receiving IBAN " ]
            , H.input [ E.onInput TransferIbanUpdated, A.type_ "text", A.class "form-control", A.value s.transferIban ] [] 
            ]
        , H.div [ A.class "input-group input-group-sm mb-3" ]
          [ H.span [ A.class "input-group-text" ] 
            [ H.text "Amount " ]
          , H.input [ E.onInput TransferAmountUpdated, A.type_ "number", A.step "0.01", A.class "form-control", A.value (String.fromFloat s.transferAmount)] [] 
        ]
        , H.div [ A.class "input-group input-group-sm mb-3" ]
          [ H.span [ A.class "input-group-text" ] 
            [ H.text "Reference " ]
          , H.input [ E.onInput TransferReferenceUpdated, A.type_ "text", A.class "form-control", A.value s.transferRef ] [] 
          ]
        
        , H.button [ E.onClick Transfer, A.class "btn btn-primary" ] [ H.text "Transfer" ]
        ]

      , H.hr [] []

      , H.div [ A.class "container mt-3" ] 
        [ H.h3 [] [ H.text "Transactions" ]
        , H.ul [ A.class "list-group" ] (List.map (\tx -> 
            H.li [ A.class "list-group-item rounded-1" ]
              [ H.p [] 
                [ H.strong [] [ H.text "Amount: "]
                , H.span [] [H.text (String.fromFloat tx.amount)]
                ]
              , H.p [] 
                [ H.strong [] [ H.text "Name: " ]
                , H.span [] [ H.text tx.name ]
                ]
              , H.p [] 
                [ H.strong [] [ H.text "Iban: " ]
                , H.span [] [ H.text tx.iban ]
                ]
              , H.p [] 
                [ H.strong [] [ H.text "Reference: " ]
                , H.span [] [ H.text tx.reference ]
                ]
              , H.p [] 
                [ H.strong [] [ H.text "Date: " ]
                , H.span [] [ H.text tx.time ]
                ]
              ]) s.account.txLines)
      ]]

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

updateAccountWithNewTX : Api.Account -> Api.TXLine -> Api.Account
updateAccountWithNewTX { details, txLines } tx = 
  let
      detailsUpd = { details | balance = details.balance + tx.amount }
      txUpd = tx :: txLines
  in
    { details = detailsUpd, txLines = txUpd }
