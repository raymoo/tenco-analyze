{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.Soku.Match (
                         Matching
                       , Match (..)
                       , PlayerName(..)
                       , PlayerHandle(..)
                       , OpponentName(..)
                       , requestToMatch
                       , matchInsert
                       , rateAccounts
                       )where

import           Control.Applicative
import           Data.Data
import           Data.Foldable       (foldMap)
import           Data.IxSet
import qualified Data.IxSet          as I
import           Data.List           (foldl')
import qualified Data.Map            as M
import           Data.Maybe          (listToMaybe, mapMaybe)
import           Data.Rating.Glicko
import           Data.SafeCopy
import           Data.Soku
import           Data.Soku.Accounts
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
    { mTime           :: UTCTime      -- ^ What was the match's time?
    , mGame           :: GameId       -- ^ Which game
    , mPlayerName     :: Text         -- ^ The name of the reporting player
    , mPlayerHandle   :: Text         -- ^ In-game name of the player
    , mOpponentName   :: Text         -- ^ The in-game name of their opponent
    , mOpponentHandle :: Maybe Text   -- ^ Tracker username of opponent
    , mMatched        :: Matching     -- ^ Has a match been found?
    , mWon            :: Bool         -- ^ Did the reporter win?
    , mPlayerChar     :: Character    -- ^ What character p1 used
    , mOppChar        :: Character    -- ^ Opponent character
    , mScore          :: (Int, Int)   -- ^ Score, Reporter - Opponent
    , pRating         :: Rating       -- ^ Player's rating when reporting
    , oRating         :: Maybe Rating -- ^ Opponent rating
    } deriving (Show, Data, Typeable)

instance Eq Match where
  m1 == m2 = compareMatch m1 m2 == EQ

instance Ord Match where
  compare = compareMatch

compareMatch :: Match -> Match -> Ordering
compareMatch m1 m2 = foldMap ($ (m1,m2)) comparisons
  where compareOn :: Ord b => (a -> b) -> a -> a -> Ordering
        compareOn f x y = compare (f x) (f y)
        comparisons =
          map (uncurry)
          [ compareOn mTime
          , compareOn mGame
          , compareOn mPlayerHandle
          , compareOn mOpponentName
          , compareOn mWon
          , compareOn mPlayerChar
          , compareOn mOppChar
          , compareOn mScore
          ]

type Name = Text

-- | Makes a match log request into a match
requestToMatch :: Account -> MatchResult -> Maybe Match
requestToMatch acc (MatchResult timestamp
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
                                 , mPlayerName     = username
                                 , mPlayerHandle   = p1Name
                                 , mOpponentName   = p2Name
                                 , mOpponentHandle = Nothing
                                 , mMatched        = Unmatched
                                 , mWon            = p1Score > p2Score
                                 , mPlayerChar     = p1Char
                                 , mOppChar        = p2Char
                                 , mScore          = (p1Score, p2Score)
                                 , pRating         = rating
                                 , oRating         = Nothing
                                 }
          username = accName acc
          rating = accRating acc
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
                   , mOpponentHandle = Just $ mPlayerName new
                   , oRating = Just $ pRating new }
           new' =
             new { mMatched = Unranked
                 , mOpponentHandle = Just $ mPlayerName match
                 , oRating = Just $ pRating match }
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

-- | Rating unknown for matches that have no known opponent
mToO :: Match -> Maybe Outcome
mToO Match { mWon = won, oRating = o } = fmap (, wlt) o
  where wlt = case won of
               True  -> W
               False -> L

advanceARating :: [Match] -> Rating -> Rating
advanceARating = advanceRating . mapMaybe mToO

-- | Factors any matched but unranked matches
rateAccounts :: M.Map a Account -> IxSet Match -> (M.Map a Account, IxSet Match)
rateAccounts accs ms = (accs', ms')
  where unranked = I.getEQ Unranked ms
        updateAcc acc = let matches = I.toList .
                                      I.getEQ (PlayerName $ accName acc) $
                                      unranked
                            r' = advanceARating matches (accRating acc)
                        in acc { accRating = r' }
        accs' = M.map updateAcc accs
        ms' = let stripped  = I.getRange Unmatched Ranked ms  -- no unranked
                  rList = map rank $ I.toList unranked
                  addMatches old = foldl' (flip I.insert) old rList
                 in addMatches stripped
        rank m = m { mMatched = Ranked }
