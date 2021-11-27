{-# LANGUAGE OverloadedStrings #-}
module View.HTML.Templates.Account where

import Data.Time.Format
import Data.Text as T
import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

import Application.DTO

accountHtml :: T.Text -> T.Text -> AccountDTO -> Html
accountHtml customerId customerName acc = docTypeHtml $ do
  H.head $ do
    H.meta ! charset "charset=utf-8"
    H.meta ! name "viewport" ! content "wwidth=device-width, initial-scale=1"
    H.link ! href "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" ! rel "stylesheet" 
    H.title "Banking"
  body $ do
    H.script ! src "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" $ ""

    H.h1 (toHtml customerName)
    H.h2 (toHtml $ accountDetailIban (accountDetails acc) <> " (" <> accountDetailType (accountDetails acc) <> ")")
    H.h3 (toHtml $ "Balance: " <> (show $ accountDetailBalance (accountDetails acc)))

    H.a ! class_ "btn btn-outline-primary" ! href (toValue $ "/customer?id=" <> customerId) $ "Back"

    H.hr

    H.div ! class_ "container-fluid mt-1 p-2 bg-light text-dark rounded-3" $ do
      H.form ! action "/account/deposit" ! method "post" $ do
        H.input ! type_ "hidden" ! Text.Blaze.Html5.Attributes.id "customerId" ! name "customerId" ! value (toValue customerId)
        H.input ! type_ "hidden" ! Text.Blaze.Html5.Attributes.id "customerName" ! name "customerName" ! value (toValue customerName)
        H.input ! type_ "hidden" ! Text.Blaze.Html5.Attributes.id "iban" ! name "iban" ! value (toValue (accountDetailIban (accountDetails acc)))

        H.input ! class_ "btn btn-primary" ! type_ "submit" ! Text.Blaze.Html5.Attributes.id "submit" ! value "Deposit"
        H.input ! type_ "number" ! step "0.01" ! Text.Blaze.Html5.Attributes.id "amount" ! name "amount" ! value "0.0"

    H.div ! class_ "container-fluid mt-3 p-2 bg-light text-dark rounded-3" $ do
      H.form ! action "/account/withdraw" ! method "post" $ do
        H.input ! type_ "hidden" ! Text.Blaze.Html5.Attributes.id "customerId" ! name "customerId" ! value (toValue customerId)
        H.input ! type_ "hidden" ! Text.Blaze.Html5.Attributes.id "customerName" ! name "customerName" ! value (toValue customerName)
        H.input ! type_ "hidden" ! Text.Blaze.Html5.Attributes.id "iban" ! name "iban" ! value (toValue (accountDetailIban (accountDetails acc)))

        H.input ! class_ "btn btn-primary" ! type_ "submit" ! Text.Blaze.Html5.Attributes.id "submit" ! value "Withdraw"
        H.input ! type_ "number" ! step "0.01" ! Text.Blaze.Html5.Attributes.id "amount" ! value "0.0"

    H.div ! class_ "container-fluid mt-3 p-2 bg-light text-dark rounded-3" $ do
      H.form ! action "/account/transfer" ! method "post" $ do
        H.input ! type_ "hidden" ! Text.Blaze.Html5.Attributes.id "customerId" ! name "customerId" ! value (toValue customerId)
        H.input ! type_ "hidden" ! Text.Blaze.Html5.Attributes.id "customerName" ! name "customerName" ! value (toValue customerName)
        H.input ! type_ "hidden" ! Text.Blaze.Html5.Attributes.id "fromIban" ! name "fromIban" ! value (toValue (accountDetailIban (accountDetails acc)))

        H.div ! class_ "input-group input-group-sm mb-3" $ do
          H.span ! class_ "input-group-text" $ "Receiving IBAN"
          H.input ! class_ "form-control" ! type_ "text" ! Text.Blaze.Html5.Attributes.id "receivingIban" ! name "receivingIban"

        H.div ! class_ "input-group input-group-sm mb-3" $ do
          H.span ! class_ "input-group-text" $ "Amount"
          H.input ! class_ "form-control" ! type_ "number" ! step "0.01" ! Text.Blaze.Html5.Attributes.id "amount" ! name "amount" ! value "0.0"

        H.div ! class_ "input-group input-group-sm mb-3" $ do
          H.span ! class_ "input-group-text" $ "Reference"
          H.input ! class_ "form-control" ! type_ "text" ! Text.Blaze.Html5.Attributes.id "reference" ! name "reference"

        H.input ! class_ "btn btn-primary" ! type_ "submit" ! Text.Blaze.Html5.Attributes.id "submit" ! value "Transfer"

    H.hr

    H.div ! class_ "container mt-3" $ do
      H.h3 "Transactions"
      ul ! class_ "list-group" $ do
        forM_ (accountTXLines acc) renderTxLine

renderTxLine :: TXLineDTO -> Html
renderTxLine tx = do
  H.li ! class_ "list-group-item rounded-1" $ do
    H.p $ do
      H.strong "Amount: "
      H.span $ toHtml $ txLineAmount tx

    H.p $ do
      H.strong "Name: "
      H.span $ toHtml $ txLineName tx

    H.p $ do
      H.strong "Iban: "
      H.span $ toHtml $ txLineIban tx

    H.p $ do
      H.strong "Reference: "
      H.span $ toHtml $ txLineReference tx

    H.p $ do
      H.strong "Date: "
      H.span $ toHtml $ formatTime defaultTimeLocale "%d.%m.%Y %H:%M" (txLineTime tx)