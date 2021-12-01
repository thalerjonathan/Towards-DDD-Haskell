module Page.Customer exposing (Model, Msg, init, subscriptions, update, view)

import Http
import Html as H exposing (Html)
import Html.Attributes as A

import Api
import Page.Utils as Utils

type alias CustomerId = String

type Model 
  = Loading CustomerId
  | Failure CustomerId String
  | Success Api.Customer

type Msg
  = CustomerLoaded (Result Http.Error Api.Customer)

init : CustomerId -> (Model, Cmd Msg)
init cid = 
  ( Loading cid
  , Http.get
      { url = "/rest/customer/" ++ cid
      , expect = Http.expectJson CustomerLoaded Api.customerDecoder
      }
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model) of
    (CustomerLoaded result, Loading cd) ->
      case result of
        Ok c ->
          (Success c, Cmd.none)

        Err err ->
          (Failure cd (Utils.errorToString err), Cmd.none)
    (_, _) ->
      (model, Cmd.none)

view : Model -> List (Html Msg)
view model =
  case model of 
    Loading cid -> 
      [ H.h2 [] [ H.text ("Loading customer " ++ cid ++ "...") ]]
    
    Failure cid err -> 
      [ H.h2 [] [ H.text ("Failed loading customer " ++ cid ++ ": '" ++ err ++ "'") ]]
    
    Success c ->
      [ H.h1 [] [ H.text c.details.name ]
      , H.a [ A.class "btn btn-outline-primary", A.href "/spa/" ] [ H.text "Back"]
      , H.br [] []
      , H.br [] []
      , H.ul [ A.class "list-group" ] (List.map (\acc -> 
          H.li [ A.class "list-group-item d-flex justify-content-between align-items-center" ] [ 
              H.a [ A.href ("/spa/account/" ++ acc.iban ++ "/" ++ c.details.id ++ "/" ++ c.details.name) ] 
                [ H.text (acc.iban ++ " (" ++ acc.accountType ++ ")") ] ]) c.accountDetails)
      ]

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none