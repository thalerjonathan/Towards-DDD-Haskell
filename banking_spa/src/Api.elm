module Api exposing (..)

import Json.Decode as D
import Either as E
import Either.Decode as ED

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

type alias Account = 
  { details : AccountDetails
  , txLines : List TXLine
  }

type alias TXLine =
  { iban      : String
  , name      : String
  , reference : String
  , amount    : Float
  , time      : String
  }

type alias TxCommandResponse 
  = E.Either String TXLine

commandResponseDecoder : D.Decoder TxCommandResponse
commandResponseDecoder = 
  ED.either 
    (D.field "Left" D.string) 
    (D.field "Right" txLineDecoder)

customerDetailsDecoder : D.Decoder CustomerDetails
customerDetailsDecoder =
  D.map2 CustomerDetails
    (D.field "customerDetailsId" D.string)
    (D.field "customerDetailsName" D.string)

customerDecoder : D.Decoder Customer
customerDecoder =
  D.map2 Customer 
    (D.field "customerDetails" customerDetailsDecoder)
    (D.field "customerAccountDetails" (D.list accountDetailsDecoder))

accountDetailsDecoder : D.Decoder AccountDetails
accountDetailsDecoder =
  D.map3 AccountDetails
    (D.field "accountDetailIban" D.string)
    (D.field "accountDetailBalance" D.float)
    (D.field "accountDetailType" D.string)

accountDecoder : D.Decoder Account
accountDecoder =
  D.map2 Account
    (D.field "accountDetails" accountDetailsDecoder)
    (D.field "accountTXLines" (D.list txLineDecoder))

txLineDecoder : D.Decoder TXLine
txLineDecoder 
  = D.map5 TXLine
    (D.field "txLineIban" D.string)
    (D.field "txLineName" D.string)
    (D.field "txLineReference" D.string)
    (D.field "txLineAmount" D.float)
    (D.field "txLineTime" D.string)

