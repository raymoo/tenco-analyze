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

module Data.Soku.Requests ( ISOTime
                          , NewAccountReq(..)
                          , MatchResult(..)
                          , ReportLog(..)
                          )where

import Data.Soku
import Data.Text (Text)

type ISOTime = String

-- | A request for a new account
data NewAccountReq = NewAccountReq
    { newName     :: Text -- ^ Username
    , newPassword :: Text -- ^ Password
    , newMail     :: Text -- ^ Email
    } deriving (Show)

data MatchResult = MatchResult
    { mrTimestamp :: ISOTime   -- ^ Timestamp of the match, from tsk
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
