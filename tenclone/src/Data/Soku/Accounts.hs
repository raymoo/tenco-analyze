{-
    Copyright 2014 Leon Medvinsky

    This file is part of Tenclone.

    Tenclone is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Tenclone is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Tenclone.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module Data.Soku.Accounts(
                           Account(..)
                         , reqToAcc
                         , AccountList(..)
                         , register
                         , emptyAccList
                         , RegError(..)
                         , LoginError(..)
                         , tryLogin
                         ) where

import           Crypto.Hash
import           Data.ByteString.Char8 as BS (pack)
import           Data.Data
import           Data.Map              as Map (Map, empty, insert, lookup,
                                               member, fromAscList)
import           Data.Rating.Glicko
import           Data.SafeCopy
import           Data.Soku
import           Data.Soku.Requests
import           Data.Text             as T

-- | Represents someone's account information
data Account =
  Account { accName   :: Text
          , accPass   :: Text
          , accMail   :: Text
          , accRating :: Map Character Rating
          } 
  deriving (Show, Data, Typeable)

instance Eq Account where
  a1 == a2 = accName a1 == accName a2

instance Ord Account where
  compare a1 a2 = compare (accName a1) (accName a2)

deriveSafeCopy 0 'base ''Account

-- | List of registered accounts
data AccountList = AccountList { accList :: Map.Map T.Text Account }
                   deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base  ''AccountList)

-- | Possible errors when trying to register
data RegError = ExistingAccount
              | EmptyName
              | EmptyPass
                deriving (Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''RegError)

-- | Returns the updated set for a successful registration, otherwise gives
-- an error.
register :: AccountList -> Account -> Either RegError AccountList
register (AccountList as) a
    | accName a == ""             = Left EmptyName
    | accPass a == ""             = Left EmptyPass
    | accName a `Map.member` as   = Left ExistingAccount
    | otherwise                   = Right $ AccountList (Map.insert (accName a) a as)

-- | The starting list
emptyAccList :: AccountList
emptyAccList = AccountList Map.empty

data LoginError = NonexistentAccount
                | WrongPassword
                  deriving (Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''LoginError)

tryLogin :: AccountList ->
            T.Text      -> -- ^ Username
            T.Text      -> -- ^ Password
            Either LoginError Account
tryLogin (AccountList as) n p =
    case Map.lookup n as of
      Nothing  -> Left NonexistentAccount
      Just acc -> if T.unpack p == makePasswordHash (BS.pack . T.unpack $ accPass acc)
                  then Right acc
                  else Left WrongPassword
  where makePasswordHash bs = show (hash bs :: Digest SHA1)

reqToAcc :: NewAccountReq -> Account
reqToAcc (NewAccountReq name pass mail) =
  Account { accName   = name
          , accPass   = pass
          , accMail   = mail
          , accRating =
              fromAscList $ Prelude.zip [minBound..maxBound] (repeat defRating)
          } 
