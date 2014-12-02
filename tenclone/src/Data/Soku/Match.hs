{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.Soku.Match (
                         Matching
                       , Match (..)
                       , PlayerName(..)
                       , PlayerHandle(..)
                       , OpponentName(..)
                       , requestToMatch
                       , matchInsert
                       )where

import           Control.Applicative
import           Data.Data
import           Data.IxSet
import qualified Data.IxSet          as I
import           Data.Maybe          (listToMaybe)
import           Data.SafeCopy
import           Data.Soku
import           Data.Soku.Requests
import           Data.Text           (Text)
import           Data.Time           (UTCTime, addUTCTime)
import           Data.Time.ISO8601   (parseISO8601)
import           Data.Tuple          (swap)

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
    { mTime           :: UTCTime    -- ^ What was the match's time?
    , mGame           :: GameId     -- ^ Which game
    , mPlayerName     :: Text       -- ^ The name of the reporting player
    , mPlayerHandle   :: Text       -- ^ In-game name of the player
    , mOpponentName   :: Text       -- ^ The in-game name of their opponent
    , mOpponentHandle :: Maybe Text -- ^ Tracker username of opponent
    , mMatched        :: Matching   -- ^ Has a match been found?
    , mWon            :: Bool       -- ^ Did the reporter win?
    , mPlayerChar     :: Character  -- ^ What character p1 used
    , mOppChar        :: Character  -- ^ Opponent character
    , mScore          :: (Int, Int) -- ^ Score, Reporter - Opponent
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
    where madeMatch t g =  Match { mTime           = t
                                 , mGame           = g
                                 , mPlayerName     = user
                                 , mPlayerHandle   = p1Name
                                 , mOpponentName   = p2Name
                                 , mOpponentHandle = Nothing
                                 , mMatched        = Unmatched
                                 , mWon            = p1Score > p2Score
                                 , mPlayerChar     = p1Char
                                 , mOppChar        = p2Char
                                 , mScore          = (p1Score, p2Score)
                                 }
$(deriveSafeCopy 0 'base ''Match)

newtype PlayerName = PlayerName Text
    deriving (Eq, Ord, Show, Data, Typeable)

deriveSafeCopy 0 'base ''PlayerName

newtype PlayerHandle = PlayerHandle Text
    deriving (Eq, Ord, Show, Data, Typeable)

deriveSafeCopy 0 'base ''PlayerHandle

newtype OpponentName = OpponentName Text
    deriving (Eq, Ord, Show, Data, Typeable)

deriveSafeCopy 0 'base ''OpponentName

newtype PChar = PChar Character
    deriving (Eq, Ord, Show, Data, Typeable)

deriveSafeCopy 0 'base ''PChar

newtype OChar = OChar Character
    deriving (Eq, Ord, Show, Data, Typeable)

deriveSafeCopy 0 'base ''OChar

instance Indexable Match where
    empty = ixSet [ ixFun $ \m -> [mTime m]
                  , ixFun $ \m -> [mGame m]
                  , ixFun $ \m -> [PlayerName $ mPlayerName m]
                  , ixFun $ \m -> [PlayerHandle $ mPlayerHandle m]
                  , ixFun $ \m -> [OpponentName $ mOpponentName m]
                  , ixFun $ \m -> [PChar $ mPlayerChar m]
                  , ixFun $ \m -> [OChar $ mOppChar m]
                  , ixFun $ \m -> [mMatched m]
                  , ixFun $ \m -> [mPlayerChar m]
                  , ixFun $ \m -> [mScore m]
                  ]

-- | Insert a match, updating any matches (as in matching matches)
matchInsert :: Match -> IxSet Match -> IxSet Match
matchInsert new set =
  case possibleMatch of
   Nothing    -> I.insert new set
   Just match -> I.insert updated .
                 I.insert new' .
                 I.delete match $ set
     where updated =
             match { mMatched = Unranked
                   , mOpponentHandle = Just $ mPlayerName new }
           new' =
             new { mMatched = Unranked
                 , mOpponentHandle = Just $ mPlayerName match }
  where timeRange = (addUTCTime (-60) newTime, addUTCTime 60 newTime)
        newTime = mTime new
        possibleMatch = listToMaybe .
                        I.toAscList (I.Proxy :: I.Proxy UTCTime) .
                        -- Check if the names match
                        I.getEQ (OpponentName $ mPlayerHandle new) .
                        I.getEQ (PlayerHandle $ mOpponentName new) .
                        -- Check if it's the same game type
                        I.getEQ (mGame new) .
                        -- Check if the characters are the same
                        I.getEQ (PChar $ mOppChar new) .
                        I.getEQ (OChar $ mPlayerChar new) .
                        -- Check if the scores are the same
                        I.getEQ (swap $ mScore new) $
                        -- Filter to times within a minute
                        (uncurry I.getRange timeRange set)
