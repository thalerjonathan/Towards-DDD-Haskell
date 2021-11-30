module Api exposing (..)

import Json.Decode as D

type alias CustomerDetails = 
  { id   : String
  , name : String
  }

type alias Customer = 
  { details        : CustomerDetails
  , accountDetails : List AccountDetails
  }
  
type alias AccountDetails = 
  { iban        : String
  , balance     : Float
  , accountType : String
  }

customerDetailsDecoder : D.Decoder CustomerDetails
customerDetailsDecoder =
  D.map2 CustomerDetails
      (D.field "customerDetailsId" D.string)
      (D.field "customerDetailsName" D.string)

customerDecoder : D.Decoder Customer
customerDecoder =
  D.map2 Customer 
      (D.field "customerDetails" customerDetailsDecoder)
      (D.list accountDetailsDecoder)

accountDetailsDecoder : D.Decoder AccountDetails
accountDetailsDecoder =
  D.map3 AccountDetails
      (D.field "accountDetailIban" D.string)
      (D.field "accountDetailBalance" D.float)
      (D.field "accountDetailType" D.string)