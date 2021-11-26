{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module View.Rest.Generator where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

rest :: QuasiQuoter
rest =  QuasiQuoter {
    quoteExp  = undefined,
    quotePat  = undefined,
    quoteType = undefined,
    quoteDec  = quoteFuncInfoDec
  }

quoteFuncInfoDec :: String -> Q [Dec]
quoteFuncInfoDec _quote = do
  -- reportWarning $ "quoteFuncInfoDec: " ++ quote
  return []

genServer :: [Name] -> Q [Dec]
genServer _names = do
  -- reportWarning $ "genServer: " ++ show names
  return []