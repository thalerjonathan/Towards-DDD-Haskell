module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Html as H
import Html.Attributes as A
import Route exposing (Route)
import Router exposing (Config, Router)
import Url exposing (Url)

-- MODEL
type alias Model =
    { router : Router Route }

-- MSG
type Msg
    = Router (Router.Msg Route.Msg)
    | UrlChanged Url

-- ROUTER CONFIG
config : Config Msg Route Route.Msg
config =
    { parser = Route.parser
    , update = Route.update
    , view = Route.view
    , message = Router
    , subscriptions = Route.subscriptions
    , notFound = Route.notFound
    , routeTitle = Route.title
    , onUrlChanged = Just UrlChanged
    }

-- INIT
init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
  let
    ( router, cmd ) =
        Router.init config url key
  in
    ( Model router, cmd )

-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update message ({ router } as model) =
  case message of
    Router msg ->
      let
        ( newRouter, cmd ) =
          Router.update config msg router
      in
      ( { model | router = newRouter }, cmd )

    UrlChanged url ->
        ( model, Cmd.none )

-- VIEW
view : Model -> Document Msg
view { router } =
  { title = Router.title router "Banking"
  , body =
    [ H.nav []
      [ H.a [ A.href "/" ] [ H.text "All Customers" ]
      , H.a [ A.href "/customer" ] [ H.text "Customer" ]
      , H.a [ A.href "/account" ] [ H.text "Account" ]
      , H.a [ A.href "/something_not_routed" ] [ H.text "404" ]
      ]
    , H.main_ [] (Router.view config router)
    ]
  }

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions { router } =
  Router.subscriptions config router

-- MAIN
main : Program () Model Msg
main =
  Browser.application
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    , onUrlChange = Router.onUrlChange config
    , onUrlRequest = Router.onUrlRequest config
    }
