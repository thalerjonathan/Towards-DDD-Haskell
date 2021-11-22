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

import           Database.Persist.Postgresql
import           Database.Persist.Quasi
import           Database.Persist.TH
import           Data.Text

-- import Data.Time

share [mkPersist sqlSettings]
    $(persistFileWith lowerCaseSettings "db/banking.persistentmodels")

{-
testPGQuery :: PgPool -> IO ()
testPGQuery p = runSqlPool act (getPgPool p)
  where
    act = do
      rs <- selectList [] [LimitTo 10]
      mapM_ (\r -> liftIO $ print (r :: Entity VVVUser)) rs

allVVVUsers :: PgPool -> IO [Entity VVVUser]
allVVVUsers p = runSqlPool act (getPgPool p)
  where
    act = do selectList [] [LimitTo 10]

vvvUserByName :: PgPool -> Text -> IO (Maybe (Entity VVVUser))
vvvUserByName p name = runSqlPool act (getPgPool p)
  where
    act = do 
      ret <- selectList [VVVUserName ==. name] [LimitTo 1]
      if Prelude.null ret
        then return Nothing
        else return $ Just (Prelude.head ret)

createVVVUser :: PgPool -> Text -> Text -> Text -> IO (Entity VVVUser)
createVVVUser p name email password = do
    let u = VVVUser 1 name email password False Nothing False Nothing Nothing
    let act = insert u
    key <- runSqlPool act (getPgPool p)
    return $ Entity key u

vvvUserNameFromEntity :: Entity VVVUser -> Text
vvvUserNameFromEntity (Entity _key e) = vVVUserName e

vvvUserEmailFromEntity :: Entity VVVUser -> Text
vvvUserEmailFromEntity (Entity _key e) = vVVUserEmail e

vvvUserPasswordFromEntity :: Entity VVVUser -> Text
vvvUserPasswordFromEntity (Entity _key e) = vVVUserPassword e

vvvUserIsEnabledFromEntity :: Entity VVVUser -> Bool
vvvUserIsEnabledFromEntity (Entity _key e) = vVVUserEnabled e

vvvUserIsAdminFromEntity :: Entity VVVUser -> Bool
vvvUserIsAdminFromEntity (Entity _key e) = vVVUserAdmin e

vvvUserActivationTokenFromEntity :: Entity VVVUser -> Maybe Text
vvvUserActivationTokenFromEntity (Entity _key e) = vVVUserActivationToken e

vvvUserRefereeIdFromEntity :: Entity VVVUser -> Maybe Int
vvvUserRefereeIdFromEntity (Entity _key e) = vVVUserRefereeId e

vvvUserClubIdFromEntity :: Entity VVVUser -> Maybe Int
vvvUserClubIdFromEntity (Entity _key e) = vVVUserClubId e

-- TODO: use toSqlKey/fromSqlKey
extractInt64KeyFromEntity :: Entity VVVUser -> Maybe Int64
extractInt64KeyFromEntity (Entity key _) = extractInt64 $ keyToValues key 

extractInt64Key :: PersistEntity a => Key a -> Maybe Int64
extractInt64Key key = extractInt64 $ keyToValues key 

extractInt64 :: [PersistValue] -> Maybe Int64
extractInt64 [PersistInt64 i] = Just i
extractInt64 _ = Nothing
  -}