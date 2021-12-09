module View.HTML.Page.Error where

import Data.Text
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

errorPageHtml :: Text -> Html
errorPageHtml msg = docTypeHtml $ do
  H.head $ do
    H.meta ! charset "charset=utf-8"
    H.meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    H.link ! href "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" ! rel "stylesheet" 
    H.title "Banking"
  body $ do
    H.script ! src "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" $ ""

    H.div ! class_ "alert alert-danger" $
      H.h1 (toHtml $ "Error: " <> msg)