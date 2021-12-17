module View.HTML.Page.Error where

import           Data.Text
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes
import           View.HTML.Page.Head

errorPageHtml :: Text -> Html
errorPageHtml msg = docTypeHtml $ do
  bankingHead
  body $ do
    H.script ! src "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" $ ""

    H.div ! class_ "alert alert-danger" $
      H.h1 (toHtml $ "Error: " <> msg)
