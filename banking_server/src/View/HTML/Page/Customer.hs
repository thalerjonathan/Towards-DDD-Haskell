{-# LANGUAGE OverloadedStrings #-}
module View.HTML.Page.Customer where

import           Control.Monad               (forM_)
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes

import           Application.DTO
import           View.HTML.Page.Head

customerHtml :: CustomerDTO -> Html
customerHtml c = docTypeHtml $ do
  bankingHead
  body $ do
    H.script ! src "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" $ ""

    H.h1 (toHtml $ customerDetailsName $ customerDetails c)

    H.a ! class_ "btn btn-outline-primary" ! href "/" $ "Back"

    H.br
    H.br

    ul ! class_ "list-group" $ forM_ (customerAccountDetails c) (renderAccount c)

renderAccount :: CustomerDTO -> AccountDetailsDTO -> Html
renderAccount c acc = do
  H.li ! class_ "list-group-item d-flex justify-content-between align-items-center" $ do
    H.a ! href (toValue $ "/account?iban=" <> accountDetailIban acc <>
                          "&id=" <> customerDetailsId (customerDetails c) <>
                          "&name=" <> customerDetailsName (customerDetails c)) $
                            toHtml $ accountDetailIban acc <> " (" <> accountDetailType acc <> ")"

    H.span ! class_ "badge bg-primary rounded-pill" $ toHtml $ accountDetailBalance acc

