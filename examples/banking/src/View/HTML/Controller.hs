module View.HTML.Controller where
  
import Infrastructure.Cache.AppCache (AppCache)
import Infrastructure.DB.Pool (DbPool)
import Servant (Handler)
import View.HTML.Templates.AllCustomers (allCustomers)
import Text.Blaze.Html
import Control.Monad.IO.Class
  
getAllCustomers :: AppCache
                -> DbPool 
                -> Handler Html
getAllCustomers _cache _p = do
  -- liftIO $ Pool.runWithTX p (getAllCustomers cache)
  liftIO $ putStrLn "getAllCustomers"
  return (allCustomers 10)