{-# LANGUAGE OverloadedStrings #-}
module View.HTML.Templates.AllCustomers where

import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Application.DTO
import Text.Blaze.Html5.Attributes

allCustomersHtml :: [CustomerDetailsDTO] -> Html
allCustomersHtml cs = docTypeHtml $ do
  H.head $ do
    H.meta ! charset "charset=utf-8"
    H.meta ! name "viewport" ! content "wwidth=device-width, initial-scale=1"

    H.link ! href "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" ! rel "stylesheet" 

    H.title "Banking"
  body $ do
    H.script ! src "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" $ ""

    H.h1 "Customers"

    ul ! class_ "list-group list-group-flush" $ forM_ cs renderCustomer

renderCustomer :: CustomerDetailsDTO -> Html
renderCustomer c = do
  H.li ! class_ "list-group-item" $
    H.a ! href (toValue $ "/customer?id=" <> (customerDetailsId c)) $ toHtml $ customerDetailsName c