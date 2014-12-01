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
{-# LANGUAGE TemplateHaskell    #-}
module Data.Soku.Requests ( ISOTime
                          , NewAccountReq(..)
                          , MatchResult(..)
                          , ReportLog(..)
                          )where

import           Data.Data
import           Data.SafeCopy
import           Data.Soku
import           Data.Text     (Text)

type ISOTime = String

-- | A request for a new account
data NewAccountReq = NewAccountReq
    { newName     :: Text -- ^ Username
    , newPassword :: Text -- ^ Password
    , newMail     :: Text -- ^ Email
    } deriving (Show, Data, Typeable)

-- This is necessary for being used in acid-state
$(deriveSafeCopy 0 'base ''NewAccountReq)

instance Eq NewAccountReq where
    a1 == a2 = newName a1 == newName a2

instance Ord NewAccountReq where
    compare a1 a2 = compare (newName a1) (newName a2)

data MatchResult = MatchResult
    { mrTimestamp :: ISOTime   -- ^ Timestamp of the match, from tsk
    , mrGame      :: Text      -- ^ Game id field
    , mrP1Name    :: Text      -- ^ Name of player 1
    , mrP1Char    :: Character -- ^ Which character player 1 used
    , mrP1Score   :: Int       -- ^ How many rounds player 1 won
    , mrP2Name    :: Text
    , mrP2Char    :: Character
    , mrP2Score   :: Int
    } deriving (Show)


data ReportLog = ReportLog
    { rlName   :: Text          -- ^ Account name
    , rlPass   :: Text          -- ^ Account password
    , rlGameId :: GameId        -- ^ Which game they played
    , rlRecord :: [MatchResult] -- ^ The log of matches
    , rlForce  :: Bool          -- ^ Whether to force insert
    } deriving (Show)
