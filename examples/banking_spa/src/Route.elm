module Route exposing (..)

import Html as H exposing (Html)
import Page.Account as Account
import Page.Customer as Customer
import Page.AllCustomers as AllCustomers
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)
import Url exposing (Url)

-- MODEL
type Route
  = AllCustomersRoute
  | AccountRoute String
  | CustomerRoute String

-- PARSER
parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map AllCustomersRoute Parser.top
        , Parser.map CustomerRoute (s "customer" </> Customer.urlParser)
        , Parser.map AccountRoute (s "account" </> Account.urlParser)
        ]
      
fromUrl : Url -> Maybe Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser
