module Route exposing (..)

import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)
import Url exposing (Url)

-- MODEL
type Route
  = AllCustomers
  | Account String String String
  | Customer String

-- PARSER
routeParser : Parser (Route -> a) a
routeParser = 
    oneOf
        [ Parser.map AllCustomers (s "spa") 
        , Parser.map AllCustomers (s "spa" </> s "index.html") 
        , Parser.map Customer (s "spa" </> s "customer" </> string)
        , Parser.map Account (s "spa" </> s "account" </> string </> string </> string )
        ]
      
fromUrl : Url -> Maybe Route
fromUrl url = Parser.parse routeParser url