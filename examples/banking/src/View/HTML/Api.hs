{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
module View.HTML.Api where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html (Html)

type GetAllCustomersHtml = Get '[HTML] Html