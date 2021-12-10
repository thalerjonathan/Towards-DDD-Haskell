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
    createCache AccountCache (Just 60)
    createCache CustomerCache (Just 60)
    createCache TxLineCache (Just 60)
  where
    createCache :: CacheRegion -> Maybe Int -> StateT AppCache IO ()
    createCache region dur = do
      let tspec = fmap (\s -> TimeSpec (fromIntegral s) 0) dur :: Maybe TimeSpec
      c <- liftIO $ newCache tspec
      modify (Map.insert region c)

evictCacheRegion :: AppCache -> CacheRegion -> IO ()
evictCacheRegion cache region = do
  -- TODO: make safe without fromJust
  let regionCache = (fromJust . Map.lookup region) cache
  ks <- Cache.keys regionCache
  forM_ ks (Cache.delete regionCache)

performCachedAction :: AppCache -> CacheRegion -> String -> IO a -> IO a
performCachedAction cache region entryKey act = do
  -- TODO: make safe without fromJust
  let regionCache = (fromJust . Map.lookup region) cache
  mFixCache <- Cache.lookup (unsafeCoerce regionCache) entryKey
  case mFixCache of
    Nothing -> do
      ret <- act
      Cache.insert regionCache entryKey (unsafeCoerce ret)
      return ret
    (Just fixCache) -> return fixCache
