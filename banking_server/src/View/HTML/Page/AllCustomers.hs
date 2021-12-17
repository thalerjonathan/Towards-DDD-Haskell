{-# LANGUAGE OverloadedStrings #-}
module View.HTML.Page.AllCustomers where

import           Application.DTO
import           Control.Monad               (forM_)
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes
import           View.HTML.Page.Head

allCustomersHtml :: [CustomerDetailsDTO] -> Html
allCustomersHtml cs = docTypeHtml $ do
  bankingHead
  body $ do
    H.script ! src "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" $ ""

    H.h1 "Customers"

    ul ! class_ "list-group list-group-flush" $ forM_ cs renderCustomer

renderCustomer :: CustomerDetailsDTO -> Html
renderCustomer c = do
  H.li ! class_ "list-group-item" $
    H.a ! href (toValue $ "/customer?id=" <> (customerDetailsId c)) $ toHtml $ customerDetailsName c
