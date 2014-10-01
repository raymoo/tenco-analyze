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

{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Data.Soku.Accounts(
                           Account
                         , accName
                         , accPass
                         , accMail
                         , AccountList(..)
                         , register
                         , emptyAccList
                         , RegError(..)
                         , LoginError(..)
                         , tryLogin
                         ) where

import Data.Map as Map (Map, member, insert, empty, lookup, keys)
import Data.Soku.Requests
import Data.Text as T
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative ((<$>), (<*>), pure)
import Data.Data
import Data.Acid (AcidState, Query, Update, makeAcidic, query, update)
import Data.SafeCopy 
import Crypto.Hash
import Data.ByteString.Char8 as BS (pack)

-- | Represents someone's account information
type Account = NewAccountReq
accName = newName
accPass = newPassword
accMail = newMail

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
            Maybe LoginError
tryLogin (AccountList as) n p =
    case Map.lookup n as of
      Nothing  -> Just NonexistentAccount
      Just acc -> if T.unpack p == makePasswordHash (BS.pack . T.unpack $ accPass acc)
                  then Nothing
                  else Just WrongPassword
  where makePasswordHash bs = show (hash bs :: Digest SHA1)
