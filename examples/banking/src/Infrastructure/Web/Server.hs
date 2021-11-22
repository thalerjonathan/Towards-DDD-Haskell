module Infrastructure.Web.Server
  ( startServer
  ) where

import Network.Wai
import Network.Wai.Handler.Warp

-- development serves over port 3000 (for react)
port :: Int
port = 8080

-- developement always over normal HTTP
startServer :: Application -> IO ()
startServer = run port