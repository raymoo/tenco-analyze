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
                         , emptyAccList
                         , RegError(..)
                         , registerAccount
                         , findAccount
                         ) where

import Data.Map as Map (Map, member, insert, empty, lookup)
import Data.Soku.Requests
import Data.Text as T
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative ((<$>), (<*>), pure)
import Data.Data
import Data.Acid (AcidState, Query, Update, makeAcidic, query, update)
import Data.SafeCopy 

-- | Represents someone's account information
type Account = NewAccountReq
accName = newName
accPass = newPassword
accMail = newMail

-- | List of registered accounts
data AccountList = AccountList { accList :: Map.Map T.Text Account }
                   deriving (Data, Typeable)

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

-- | Updates the state to register an account. Returns 'Nothing' on success,
-- returns 'Just error' on failure
regUpdate :: Account -> Update AccountList (Maybe RegError)
regUpdate acc = register <$> get <*> pure acc >>= \result ->
                either (return . Just) 
                       (\list -> put list >> return Nothing)
                       result

-- | Gets the account with this name
queryAccount :: T.Text -> Query AccountList (Maybe Account)
queryAccount name = lookupAcc
    where lookupAcc = Map.lookup name . accList <$> ask

$(makeAcidic ''AccountList ['regUpdate, 'queryAccount])

registerAccount :: AcidState AccountList -> Account -> IO (Maybe RegError)
registerAccount astate acc = update astate (RegUpdate acc)

findAccount :: AcidState AccountList -> T.Text -> IO (Maybe Account)
findAccount astate name = query astate (QueryAccount name)
