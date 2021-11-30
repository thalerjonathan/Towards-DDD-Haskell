module Page.Customer exposing (Model, Msg, init, subscriptions, update, urlParser, view)

import Http
import Html as H exposing (Html)
import Html.Attributes as A
import Url.Parser

import Api
  
type alias CustomerId = String

-- MODEL
type Model 
  = Loading CustomerId
  | Failure CustomerId String
  | Success Api.Customer

type Msg
  = CustomerLoaded (Result Http.Error Api.Customer)

-- INIT
init : CustomerId -> (Model, Cmd Msg)
init cid = 
  ( Loading cid
  , Http.get
      { url = "http://localhost:8080/rest/customer/" ++ cid
      , expect = Http.expectJson CustomerLoaded Api.customerDecoder
      }
  )

-- update
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model) of
    (CustomerLoaded result, Loading cd) ->
      case result of
        Ok c ->
          (Success c, Cmd.none)

        Err err ->
          (Failure cd (errorToString err), Cmd.none)
    (_, _) ->
      (model, Cmd.none)

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
    Loading cid -> 
      [ H.h2 [] [ H.text ("Loading customer " ++ cid ++ "...") ]]
    
    Failure cid err -> 
      [ H.h2 [] [ H.text ("Failed loading customer " ++ cid ++ ": '" ++ err ++ "'") ]]
    
    Success c ->
      [ H.h1 [] [ H.text c.details.name ]
      , H.a [ A.class "btn btn-outline-primary", A.href "/" ] [ H.text "Back"]
      , H.br [] []
      , H.br [] []
      , H.ul [ A.class "list-group" ] (List.map (\acc -> 
          H.li [ A.class "list-group-item d-flex justify-content-between align-items-center" ] [ 
              H.a [ A.href ("/account/" ++ acc.iban) ] [ H.text (acc.iban ++ " (" ++ acc.accountType ++ ")") ] ]) c.accountDetails)
      ]

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


urlParser : Url.Parser.Parser (String -> a) a
urlParser =
    Url.Parser.custom "" (\str -> Just str)
