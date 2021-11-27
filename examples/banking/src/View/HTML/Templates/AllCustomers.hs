{-# LANGUAGE OverloadedStrings #-}
module View.HTML.Templates.AllCustomers where

import Control.Monad (forM_)
import Text.Blaze.Html5 as H

allCustomers :: Int -> Html
allCustomers n = docTypeHtml $ do
  H.head $ do
    H.title "Natural numbers"
  body $ do
    p "A list of natural numbers:"
    ul $ forM_ [1 .. n] (li . toHtml)