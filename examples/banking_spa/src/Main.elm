module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Browser.Navigation as Nav
import Html as H
import Route
import Url exposing (Url)
import Page.Account as Account
import Page.Customer as Customer
import Page.AllCustomers as AllCustomers

type Model 
  = AllCustomers Key AllCustomers.Model
  | Customer Key Customer.Model
  | Account Key Account.Model
  | Loading Key
  | NotFound Key

type Msg
    = GotAllCustomersMsg AllCustomers.Msg
    | GotCustomerMsg Customer.Msg
    | GotAccountMsg Account.Msg 
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest

init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key = changeRouteTo (Route.fromUrl url) (Loading key)

keyFromModel : Model -> Key
keyFromModel m = 
  case m of 
    (AllCustomers k _ ) -> k
    (Customer k _ ) -> k
    (Account k _ ) -> k
    (Loading k ) -> k
    (NotFound k ) -> k

changeRouteTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model = 
  let
    key = keyFromModel model
  in
    case maybeRoute of 
      Nothing -> 
        (NotFound key, Cmd.none)

      (Just Route.AllCustomers) ->
        AllCustomers.init 
          |> updateWith (AllCustomers key) GotAllCustomersMsg model
      
      (Just (Route.Customer cid)) ->
        Customer.init cid
          |> updateWith (Customer key) GotCustomerMsg model
      
      (Just (Route.Account iban customerId customerName)) ->
        Account.init iban customerId customerName
          |> updateWith (Account key) GotAccountMsg model
    
updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg _ ( subModel, subCmd ) =
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
          let key = keyFromModel model in
            ( model, Nav.pushUrl key (Url.toString url))

        Browser.External href ->
            ( model
            , Nav.load href
            )

    ( ChangedUrl url, _ ) ->
      changeRouteTo (Route.fromUrl url) model

    ( GotAllCustomersMsg subMsg, AllCustomers key subModel ) ->
      AllCustomers.update subMsg subModel 
        |> updateWith (AllCustomers key) GotAllCustomersMsg model

    ( GotCustomerMsg subMsg, Customer key subModel ) ->
      Customer.update subMsg subModel 
        |> updateWith (Customer key) GotCustomerMsg model

    ( GotAccountMsg subMsg, Account key subModel ) ->
      Account.update subMsg subModel 
        |> updateWith (Account key) GotAccountMsg model

    ( _, _ ) ->
      ( model, Cmd.none )

-- VIEW
view : Model -> Document Msg
view model =
  case model of 
    Loading _ -> 
      viewPage GotAllCustomersMsg "Banking" [H.h1 [] [ H.text "Loading..." ]]

    NotFound _ ->
      viewPage GotAllCustomersMsg "Banking" [ H.h1 [] [ H.text "Page not found!" ]]

    AllCustomers _ allCustomers -> 
      viewPage GotAllCustomersMsg "Banking" (AllCustomers.view allCustomers)

    Customer _ customer -> 
      viewPage GotCustomerMsg "Banking" (Customer.view customer)

    Account _ account ->
      viewPage GotAccountMsg "Banking" (Account.view account)
 
viewPage : (a -> msg) -> b -> List (H.Html a) -> { title : b, body : List (H.Html msg) }
viewPage toMsg title body =
    { title = title
    , body = List.map (H.map toMsg) body
    }

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = 
  case model of 
    NotFound _ ->
      Sub.none

    Loading _ -> 
      Sub.none

    AllCustomers _ allCustomers -> 
      Sub.map GotAllCustomersMsg (AllCustomers.subscriptions allCustomers)

    Customer _ customer -> 
      Sub.map GotCustomerMsg (Customer.subscriptions customer)

    Account _ account ->
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
