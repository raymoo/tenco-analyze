{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Data.Soku.Match (
                         Matching
                       , Match (..)
                       , PlayerName(..)
                       , PlayerHandle(..)
                       , OpponentName(..)
                       , requestToMatch
                       )where

import Data.Soku.Requests
import Data.Text (Text)
import Data.Soku
import Data.Time (UTCTime)
import Data.Time.ISO8601 (parseISO8601)
import Data.Data
import Data.IxSet
import Data.SafeCopy
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative

-- | Unmatched = no corresponding opponent report
-- Unranked = Hasn't been factored into ranking yet
-- Ranked = Has been factored into rating
data Matching = Unmatched
              | Unranked
              | Ranked
                deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Matching)

-- | Represents a match.
data Match = Match
    { mTime         :: UTCTime    -- ^ What was the match's time?
    , mGame         :: GameId     -- ^ Which game
    , mPlayerName   :: Text       -- ^ The name of the reporting player
    , mPlayerHandle :: Text       -- ^ In-game name of the player
    , mOpponentName :: Text       -- ^ The in-game name of their opponent
    , mMatched      :: Matching   -- ^ Has a match been found?
    , mWon          :: Bool       -- ^ Did the reporter win?
    , mPlayerChar   :: Character  -- ^ What character p1 used
    , mOppChar      :: Character  -- ^ Opponent character
    , mScore        :: (Int, Int) -- ^ Score, Reporter - Opponent
    } deriving (Eq, Ord, Show, Data, Typeable)

type Name = Text

-- | Makes a match log request into a match
requestToMatch :: Name -> MatchResult -> Maybe Match
requestToMatch user (MatchResult timestamp
                                 game
                                 p1Name
                                 p1Char
                                 p1Score
                                 p2Name
                                 p2Char
                                 p2Score) = madeMatch <$> 
                                            parseISO8601 timestamp <*>
                                            parseId game
    where madeMatch t g =  Match { mTime         = t
                                 , mGame         = g
                                 , mPlayerName   = user
                                 , mPlayerHandle = p1Name
                                 , mOpponentName = p2Name
                                 , mMatched      = Unmatched
                                 , mWon          = p1Score > p2Score
                                 , mPlayerChar   = p1Char
                                 , mOppChar      = p2Char
                                 , mScore        = (p1Score, p2Score)
                                 }
$(deriveSafeCopy 0 'base ''Match)

newtype PlayerName = PlayerName Text
    deriving (Eq, Ord, Show, Data, Typeable, SafeCopy)

newtype PlayerHandle = PlayerHandle Text
    deriving (Eq, Ord, Show, Data, Typeable, SafeCopy)

newtype OpponentName = OpponentName Text
    deriving (Eq, Ord, Show, Data, Typeable, SafeCopy)

instance Indexable Match where
    empty = ixSet [ ixFun $ \m -> [mTime m] 
                  , ixFun $ \m -> [mGame m]
                  , ixFun $ \m -> [PlayerName $ mPlayerName m]
                  , ixFun $ \m -> [PlayerHandle $ mPlayerHandle m]
                  , ixFun $ \m -> [OpponentName $ mOpponentName m]
                  , ixFun $ \m -> [mMatched m]
                  , ixFun $ \m -> [mPlayerChar m]
                  ]

