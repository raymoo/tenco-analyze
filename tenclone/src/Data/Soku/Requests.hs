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
