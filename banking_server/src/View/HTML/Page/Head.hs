{-# LANGUAGE OverloadedStrings #-}
module View.HTML.Page.Head where

import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes

bankingHead :: Html
bankingHead = H.head $ do
  H.meta ! charset "charset=utf-8"
  H.meta ! name "viewport" ! content "width=device-width, initial-scale=1"
  H.link ! href "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" ! rel "stylesheet"
  H.title "Banking"
