module Infrastructure.Web.Server
  ( startServer
  ) where

import Network.Wai
import Network.Wai.Handler.Warp

port :: Int
port = 8080

startServer :: Application -> IO ()
startServer = run port