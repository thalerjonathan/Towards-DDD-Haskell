module Infrastructure.Cache.AppCache where

import Data.Cache as Cache
import GHC.Base (Any)
import Data.HashMap.Strict as Map
import Control.Monad.State
import System.Clock
import Unsafe.Coerce
import Data.Maybe

-- TODO: https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Typeable.html
type AppCache = Map.HashMap String (Cache String Any)

-- TODO: load cache config from some cfg file
mkAppCache :: IO AppCache
mkAppCache = flip execStateT Map.empty $ do
    createCache "fixture" (Just 5)
    createCache "league" (Just 5)
    createCache "siteuser" (Just 5)
    createCache "team" (Just 5)
  where
    createCache :: String -> Maybe Int -> StateT AppCache IO ()
    createCache key dur = do
      let tspec = fmap (\s -> TimeSpec (fromIntegral s) 0) dur :: Maybe TimeSpec 
      c <- liftIO $ newCache tspec 
      modify (Map.insert key c)

performCachedAction :: AppCache -> String -> String -> IO a -> IO a
performCachedAction cache regionKey entryKey act = do
  let c = (fromJust . Map.lookup regionKey) cache
  -- TODO: make safe without fromJust
  mFixCache <- Cache.lookup (unsafeCoerce c) entryKey
  case mFixCache of
    Nothing -> do
      ret <- act
      Cache.insert c entryKey (unsafeCoerce ret)
      return ret
    (Just fixCache) -> return fixCache