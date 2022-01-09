module Domain.Eff.Account where

import           Domain.Types

data AccountType = Giro | Savings deriving (Show, Eq, Read)

data Account = Account 
  { _aIban    :: Iban
  , _aBalance :: Double
  , _aType    :: AccountType
  }