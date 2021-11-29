module Page.AllCustomers exposing (Model, Msg, init, subscriptions, update, view)

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Time

type alias CustomerDetails = 
  { id :: String
  , name :: String
  }
  
-- MODEL
type alias Model =
    { name : String
    , email : String
    , ticks : Int
    }

-- INIT
init : String -> String -> Model
init name email =
    Model name email 0

-- MSG
type Msg
    = Name String
    | Email String
    | Tick Time.Posix

-- update
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name name ->
            ( { model | name = name }, Cmd.none )

        Email email ->
            ( { model | email = email }, Cmd.none )

        Tick _ ->
            ( { model | ticks = model.ticks + 1 }, Cmd.none )

-- VIEW
view : Model -> List (Html Msg)
view { name, email, ticks } =
    [ H.h1 [] [ H.text "Customers" ]
    , H.ul [ A.class "list-group list-group-flush" ] (List.map (\n -> 
        H.li [ A.class "list-group-item" ] [ 
            H.a [ A.href "/customer?id=lkjalkj" ] [ H.text n]] ) ["Jonathan Thaler", "Thomas Schwarz"])
    ]

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick
