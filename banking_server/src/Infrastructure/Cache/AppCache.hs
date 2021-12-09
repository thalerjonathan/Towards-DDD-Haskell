{-# LANGUAGE DeriveGeneric #-}
module Infrastructure.Cache.AppCache where

import           Control.Monad.State
import           Data.Cache          as Cache
import           Data.HashMap.Strict as Map
import           Data.Hashable
import           Data.Maybe
import           GHC.Base            (Any)
import           GHC.Generics        (Generic)
import           System.Clock
import           Unsafe.Coerce

data CacheRegion
  = AccountCache
  | CustomerCache
  | TxLineCache
  deriving (Eq, Show, Generic)

instance Hashable CacheRegion

-- TODO: https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Typeable.html
type AppCache = Map.HashMap CacheRegion (Cache String Any)


-- TODO: load cache config from some cfg file
mkAppCache :: IO AppCache
mkAppCache = flip execStateT Map.empty $ do
    createCache AccountCache (Just 10)
    createCache CustomerCache (Just 10)
    createCache TxLineCache (Just 10)
  where
    createCache :: CacheRegion -> Maybe Int -> StateT AppCache IO ()
    createCache region dur = do
      let tspec = fmap (\s -> TimeSpec (fromIntegral s) 0) dur :: Maybe TimeSpec
      c <- liftIO $ newCache tspec
      modify (Map.insert region c)

performCachedAction :: AppCache -> CacheRegion -> String -> IO a -> IO a
performCachedAction cache region entryKey act = do
  let c = (fromJust . Map.lookup region) cache
  -- TODO: make safe without fromJust
  mFixCache <- Cache.lookup (unsafeCoerce c) entryKey
  case mFixCache of
    Nothing -> do
      ret <- act
      Cache.insert c entryKey (unsafeCoerce ret)
      return ret
    (Just fixCache) -> return fixCache
