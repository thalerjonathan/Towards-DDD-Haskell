module View.HTML.Page.Redirect where

import           Data.Text
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes

redirectToHtml :: Text -> Html
redirectToHtml l = docTypeHtml $ do
  H.head $ do
    let refreshContent = "0; url='" <> l <> "'"
    H.meta ! httpEquiv "refresh" ! content (toValue refreshContent)
    H.title "Banking"
  body $ do
    H.p $ H.a ! href (toValue l) $ "Please Follow"
