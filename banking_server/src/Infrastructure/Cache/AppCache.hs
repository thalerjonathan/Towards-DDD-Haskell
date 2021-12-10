{-# LANGUAGE DeriveGeneric #-}
module Infrastructure.Cache.AppCache 
  ( AppCache
  , CacheRegion (..)
  , mkAppCache
  , invalidateCacheRegion
  , performCachedAction
  ) where

import           Control.Monad.State
import           Data.Cache          as Cache
import           Data.HashMap.Strict as Map
import           Data.Hashable
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

invalidateCacheRegion :: AppCache -> CacheRegion -> IO ()
invalidateCacheRegion cache region = do
  let mCache = Map.lookup region cache
  maybe
    (return ())
    Cache.purge
    mCache

performCachedAction :: AppCache -> CacheRegion -> String -> IO a -> IO a
performCachedAction cache region entryKey act = do
  let mCache = Map.lookup region cache
  maybe 
    (error $ "Cache region " ++ show region ++ " not found!") 
    (\regionCache -> Cache.fetchWithCache (unsafeCoerce regionCache) entryKey (const act))
    mCache