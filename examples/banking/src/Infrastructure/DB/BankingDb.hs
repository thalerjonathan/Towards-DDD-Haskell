{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}

module Infrastructure.DB.BankingDb 
  ( Entity (..)
  , Customer (..)
  , Account (..)
  , CustomerId
  , AccountId
  ) where

import           Database.Persist.MySQL
import           Database.Persist.Quasi
import           Database.Persist.TH
import           Data.Text

-- import Data.Time

share [mkPersist sqlSettings]
    $(persistFileWith lowerCaseSettings "db/banking.persistentmodels")

{-
fetchFixtureDetailsWithinRange :: SqlBackend -> UTCTime -> UTCTime -> IO [Entity FixtureDetails]
fetchFixtureDetailsWithinRange conn (UTCTime fromDay _) (UTCTime toDay _) 
    = runSqlConn act conn
  where
    act = do selectList [FixtureDetailsDate >=. fromDay, FixtureDetailsDate <=. toDay] []

fetchFixtureById :: SqlBackend -> FixtureId -> IO (Maybe Fixture)
fetchFixtureById conn fId = runSqlConn act conn
  where
    act = do get fId

fetchLeagueById :: SqlBackend -> LeagueId -> IO (Maybe League)
fetchLeagueById conn lId = runSqlConn act conn
  where
    act = do get lId

fetchTeamById :: SqlBackend -> TeamId -> IO (Maybe Team)
fetchTeamById conn lId = runSqlConn act conn
  where
    act = do get lId

fetchSiteUserById :: SqlBackend -> SiteUserId -> IO (Maybe SiteUser)
fetchSiteUserById conn suId = runSqlConn act conn
  where
    act = do get suId
  -}