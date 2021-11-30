module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Browser.Navigation as Nav
import Html as H
import Html.Attributes as A
import Route
import Url exposing (Url)
import Page.Account as Account
import Page.Customer as Customer
import Page.AllCustomers as AllCustomers
type Model 
  = AllCustomers AllCustomers.Model
  | Customer Customer.Model
  | Account Account.Model
  | Loading
  | NotFound 
type Msg
    = GotAllCustomersMsg AllCustomers.Msg
    | GotCustomerMsg Customer.Msg
    | GotAccountMsg Account.Msg 
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest

init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
  changeRouteTo (Route.fromUrl url) Loading

changeRouteTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
  case maybeRoute of 
    Nothing -> 
      (NotFound, Cmd.none)

    (Just Route.AllCustomers) ->
      AllCustomers.init 
        |> updateWith AllCustomers GotAllCustomersMsg model
    
    (Just (Route.Customer cid)) ->
      Customer.init cid
        |> updateWith Customer GotCustomerMsg model
    
    (Just (Route.Account str)) ->
      Account.init "" ""
        |> updateWith Account GotAccountMsg model
    
updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )

-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case ( msg, model ) of
    ( ClickedLink urlRequest, _ ) ->
      case urlRequest of
        Browser.Internal url ->
          case url.fragment of
            Nothing ->
                -- If we got a link that didn't include a fragment,
                -- it's from one of those (href "") attributes that
                -- we have to include to make the RealWorld CSS work.
                --
                -- In an application doing path routing instead of
                -- fragment-based routing, this entire
                -- `case url.fragment of` expression this comment
                -- is inside would be unnecessary.
                ( model, Cmd.none )

            Just _ ->
                ( model, Cmd.none
                -- TODO get key from init, Nav.pushUrl "foo" (Url.toString url)
                )

        Browser.External href ->
            ( model
            , Nav.load href
            )

    ( ChangedUrl url, _ ) ->
      changeRouteTo (Route.fromUrl url) model

    ( GotAllCustomersMsg subMsg, AllCustomers subModel ) ->
      AllCustomers.update subMsg subModel 
        |> updateWith AllCustomers GotAllCustomersMsg model

    ( GotCustomerMsg subMsg, Customer subModel ) ->
      Customer.update subMsg subModel 
        |> updateWith Customer GotCustomerMsg model

    ( GotAccountMsg subMsg, Account subModel ) ->
      Account.update subMsg subModel 
        |> updateWith Account GotAccountMsg model

    ( _, _ ) ->
      -- Disregard messages that arrived for the wrong page.
      ( model, Cmd.none )

-- VIEW
view : Model -> Document Msg
view model =
  case model of 
    Loading -> 
      viewPage GotAllCustomersMsg "Banking" [H.h1 [] [ H.text "Loading..." ]]

    NotFound ->
      viewPage GotAllCustomersMsg "Banking" [ H.h1 [] [ H.text "Page not found!" ]]

    AllCustomers allCustomers -> 
      viewPage GotAllCustomersMsg "Banking" (AllCustomers.view allCustomers)

    Customer customer -> 
      viewPage GotCustomerMsg "Banking" (Customer.view customer)

    Account account ->
      viewPage GotAccountMsg "Banking" (Account.view account)
 
viewPage toMsg title body =
    { title = title
    , body = List.map (H.map toMsg) body
    }

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = 
  case model of 
    NotFound ->
      Sub.none

    Loading -> 
      Sub.none

    AllCustomers allCustomers -> 
      Sub.map GotAllCustomersMsg (AllCustomers.subscriptions allCustomers)

    Customer customer -> 
      Sub.map GotCustomerMsg (Customer.subscriptions customer)

    Account account ->
      Sub.map GotAccountMsg (Account.subscriptions account)

main : Program () Model Msg
main =
  Browser.application
    { init          = init
    , update        = update
    , view          = view
    , subscriptions = subscriptions
    , onUrlChange   = ChangedUrl
    , onUrlRequest  = ClickedLink
    }
