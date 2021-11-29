module Route exposing (Msg, Route, notFound, parser, subscriptions, title, update, view)

import Html as H exposing (Html)
import Page.Account as Account
import Page.Customer as Customer
import Page.AllCustomers as AllCustomers
import Router
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)

-- MODEL
type Route
    = AllCustomers AllCustomers.Model
    | Account Account.Model
    | Customer Customer.Model

-- MSG
type Msg
    = AllCustomersMsg AllCustomers.Model AllCustomers.Msg
    | CustomerMsg Customer.Model Customer.Msg
    | AccountMsg Account.Model Account.Msg
    

-- PARSER
parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Router.mapRoute Parser.top <| AllCustomers <| AllCustomers.init "" ""  
        , Router.mapRoute (Parser.s "customer") <| Customer <| Customer.init "" ""  
        , Router.mapRoute (Parser.s "account") <| Account <| Account.init "" ""
        ]

-- UPDATE
update : Msg -> ( Route, Cmd Msg )
update message =
    case message of
        AllCustomersMsg model msg ->
            AllCustomers.update msg model
                |> Router.mapUpdate AllCustomers AllCustomersMsg

        CustomerMsg model msg ->
            Customer.update msg model
                |> Router.mapUpdate Customer CustomerMsg

        AccountMsg model msg ->
            Account.update msg model
                |> Router.mapUpdate Account AccountMsg

-- VIEW
view : Route -> List (Html Msg)
view route =
    case route of
        AllCustomers mdl ->
            AllCustomers.view mdl
                |> Router.mapMsg (AllCustomersMsg mdl)

        Account mdl ->
            Account.view mdl
                |> Router.mapMsg (AccountMsg mdl)

        Customer mdl ->
            Customer.view mdl
                |> Router.mapMsg (CustomerMsg mdl)

-- SUBSCRIPTIONS
subscriptions : Route -> Sub Msg
subscriptions route =
    case route of
        AllCustomers mdl ->
            AllCustomers.subscriptions mdl
                |> Sub.map (AllCustomersMsg mdl)

        Customer mdl ->
            Customer.subscriptions mdl
                |> Sub.map (CustomerMsg mdl)

        Account mdl ->
            Account.subscriptions mdl
                |> Sub.map (AccountMsg mdl)

-- TITLE
title : Route -> Maybe String
title _ = Just "Banking"

-- NOT FOUND
notFound : Url -> List (Html msg)
notFound url =
    [ H.h1 [] [ H.text "Page not found!" ]
    , H.h3 [] [ H.text <| Url.toString url ]
    ]
