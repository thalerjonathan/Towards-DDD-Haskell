module Page.AllCustomers exposing (Model, Msg, init, subscriptions, update, view)

import Http
import Html as H exposing (Html)
import Html.Attributes as A
import Json.Decode as D

import Api

-- MODEL
type Model 
  = Loading
  | Failure String
  | Success (List Api.CustomerDetails)

type Msg
    = AllCustomersLoaded (Result Http.Error (List Api.CustomerDetails))

-- INIT
init : (Model, Cmd Msg)
init = 
  ( Loading
  , Http.request
      { method = "GET"
      , headers = []
          -- [ Http.header "Origin" "http://elm-lang.org"
          -- , Http.header "Access-Control-Request-Method" "GET"
          -- , Http.header "Access-Control-Request-Headers" "X-Custom-Header" ]
      , url = "http://localhost:8080/rest/customer/all/"
      , body = Http.emptyBody
      , expect = Http.expectJson AllCustomersLoaded (D.list Api.customerDetailsDecoder)
      , timeout = Nothing
      , tracker = Nothing
      }
  )

-- update
update : Msg -> Model -> (Model, Cmd Msg)
update msg _ =
  case msg of
    AllCustomersLoaded result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err err ->
          (Failure (errorToString err), Cmd.none)

errorToString : Http.Error -> String
errorToString e =
  case e of
    Http.BadUrl url ->
        "The URL " ++ url ++ " was invalid"
    Http.Timeout ->
        "Unable to reach the server, try again"
    Http.NetworkError ->
        "Unable to reach the server, check your network connection"
    Http.BadStatus 500 ->
        "The server had a problem, try again later"
    Http.BadStatus 400 ->
        "Verify your information and try again"
    Http.BadStatus _ ->
        "Unknown error"
    Http.BadBody errorMessage ->
        errorMessage

-- VIEW
view : Model -> List (Html Msg)
view model =
  case model of 
    Loading -> 
      [ H.h1 [] [ H.text "Customers" ]
      , H.h2 [] [ H.text "Loading..." ]]
    
    Failure err -> 
      [ H.h1 [] [ H.text "Customers" ]
      , H.h2 [] [ H.text ("Failed loading: '" ++ err ++ "'") ]]
    
    Success cs ->
      [ H.h1 [] [ H.text "Customers" ]
      , H.ul [ A.class "list-group list-group-flush" ] (List.map (\c -> 
          H.li [ A.class "list-group-item" ] [ 
              H.a [ A.href ("/customer/" ++ c.id) ] [ H.text (c.name)]] ) cs)
      ]

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none
