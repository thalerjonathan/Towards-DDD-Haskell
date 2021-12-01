module Page.AllCustomers exposing (Model, Msg, init, subscriptions, update, view)

import Http
import Html as H exposing (Html)
import Html.Attributes as A
import Json.Decode as D

import Api
import Page.Utils as Utils

type Model 
  = Loading
  | Failure String
  | Success (List Api.CustomerDetails)

type Msg
    = AllCustomersLoaded (Result Http.Error (List Api.CustomerDetails))

init : (Model, Cmd Msg)
init = 
  ( Loading
  , Http.request
      { method = "GET"
      , headers = []
      , url = "http://localhost:8080/rest/customer/all/"
      , body = Http.emptyBody
      , expect = Http.expectJson AllCustomersLoaded (D.list Api.customerDetailsDecoder)
      , timeout = Nothing
      , tracker = Nothing
      }
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg _ =
  case msg of
    AllCustomersLoaded result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err err ->
          (Failure (Utils.errorToString err), Cmd.none)

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
              H.a [ A.href ("/spa/customer/" ++ c.id) ] [ H.text (c.name)]] ) cs)
      ]

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none
