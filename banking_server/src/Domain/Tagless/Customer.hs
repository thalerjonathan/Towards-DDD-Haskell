module Domain.Tagless.Customer where
  
import           Data.Text                               as T
import           Domain.Types

data Customer = Customer 
  { _custId   :: CustomerId
  , _custName :: T.Text
  }