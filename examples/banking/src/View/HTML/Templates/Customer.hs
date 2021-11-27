{-# LANGUAGE OverloadedStrings #-}
module View.HTML.Templates.Customer where

import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

import Application.DTO

customerHtml :: CustomerDTO -> Html
customerHtml c = docTypeHtml $ do
  H.head $ do
    H.meta ! charset "charset=utf-8"
    H.meta ! name "viewport" ! content "wwidth=device-width, initial-scale=1"
    H.link ! href "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" ! rel "stylesheet" 
    H.title "Banking"
  body $ do
    H.script ! src "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" $ ""

    H.h1 (toHtml $ customerDetailsName $ customerDetails c)

    H.a ! class_ "btn btn-outline-primary" ! href "/" $ "Back"

    H.br
    H.br

    ul ! class_ "list-group" $ forM_ (customerAccountDetails c) (renderAccount c)

renderAccount :: CustomerDTO -> AccountDetailsDTO -> Html
renderAccount c a = do
  H.li ! class_ "list-group-item d-flex justify-content-between align-items-center" $
    H.a ! href (toValue $ "/account?iban=" <> (accountDetailIban a) <> "&id=" <> (customerDetailsId $ customerDetails c) <> "&name=" <> (customerDetailsName $ customerDetails c)) $ toHtml $ (accountDetailIban a) -- <> " (" <> (accountDetailType a) <> " )"

    H.span class_ "badge bg-primary rounded-pill" $ (toHtml $ accountDetailBalance a)

{-
        
        <ul class="list-group">
            <li class="list-group-item d-flex justify-content-between align-items-center" th:each="a : ${customer.accounts}">
                
                <a th:href="@{/account(
                    iban=${a.iban},
                    id=${customer.details.id},
                    name=${customer.details.name})}" th:text="${a.iban} + ' (' + ${a.type} + ')'"></a>

                <span class="badge bg-primary rounded-pill" th:text="${a.balance}"></span>
            </li>
        </ul>
    </body>
</html>
-}