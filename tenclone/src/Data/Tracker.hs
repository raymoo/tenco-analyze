{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Data.Tracker (
                      TrackerDB
                    , emptyDB
                    , registerAccount
                    , findAccount
                    , playerList
                    , insertMatch
                    , playerMatches
                    , tryToLogin
                    , entireSetGet
                    , doRatings
                    , compileRatings
                    ) where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import           Data.Acid            as A
import           Data.Data
import           Data.IxSet
import           Data.IxSet           as I
import           Data.Map             as Map
import           Data.SafeCopy
import           Data.Soku
import           Data.Soku.Accounts
import           Data.Soku.Match
import           Data.Text            (Text)
import           Data.Time            (UTCTime)

-- | All the site data
data TrackerDB = TrackerDB
    { accountDB :: !AccountList
    , matchDB   :: !(IxSet Match)
    }
                 deriving (Eq, Ord, Show, Data, Typeable)

-- | Initial state with no account or match entries
emptyDB :: TrackerDB
emptyDB = TrackerDB emptyAccList I.empty

$(deriveSafeCopy 0 'base ''TrackerDB)

-- | Gets the account with this name
queryAccount :: Text -> Query TrackerDB (Maybe Account)
queryAccount name = Map.lookup name . accList . accountDB <$> ask

-- | Updates the state to register an account. Returns 'Nothing' on success,
-- returns 'Just error' on failure
regUpdate :: Account -> Update TrackerDB (Maybe RegError)
regUpdate acc = register . accountDB <$> get <*> pure acc >>= \result ->
                either (return . Just)
                       (\list -> modify (putList list) >> return Nothing)
                       result
    where putList list tDB = tDB { accountDB = list }

-- | Attempts a login. Returns 'Just error' if there was a problem.
attemptLogin :: Text -> Text -> Query TrackerDB (Either LoginError Account)
attemptLogin n p = tryLogin . accountDB <$> ask <*> pure n <*> pure p

-- | Return a list of players
listOfPlayers :: Query TrackerDB [Text]
listOfPlayers = getKeys . accountDB <$> ask
    where getKeys (AccountList as) = Map.keys as

-- | Inserts a match into the database
addMatch :: Match -> Update TrackerDB ()
addMatch m = modify $ modifyMatch (matchInsert m)
    where modifyMatch f tracker =
              tracker { matchDB = f $ matchDB tracker }

-- | Gets the matches a player reported.
playersMatches :: Text -> GameId -> Query TrackerDB [Match]
playersMatches name gId = I.toDescList (I.Proxy :: I.Proxy UTCTime) .
                          getEQ (PlayerName name) .
                          getEQ (gId) . matchDB <$> ask

entireSet :: Query TrackerDB (IxSet Match)
entireSet = matchDB <$> ask

conductRatings :: Update TrackerDB ()
conductRatings =
  modify $ updateDB
  where updateDB (TrackerDB (AccountList as) ms) =
          let (as', ms') = rateAccounts 1 as ms
          in TrackerDB (AccountList as') ms'

ratingsCompile :: Update TrackerDB ()
ratingsCompile = modify $ updateDB
  where updateDB (TrackerDB (AccountList as) ms) =
          let (as', ms') = compileRating as ms
          in TrackerDB (AccountList as') ms'

$(makeAcidic ''TrackerDB ['regUpdate, 'queryAccount, 'listOfPlayers, 'attemptLogin, 'addMatch, 'playersMatches, 'entireSet, 'conductRatings, 'ratingsCompile])

registerAccount :: AcidState TrackerDB -> Account -> IO (Maybe RegError)
registerAccount astate acc = A.update astate (RegUpdate acc)

findAccount :: AcidState TrackerDB -> Text -> IO (Maybe Account)
findAccount astate name = query astate (QueryAccount name)

playerList :: AcidState TrackerDB -> IO [Text]
playerList astate = query astate ListOfPlayers

tryToLogin :: AcidState TrackerDB -> Text -> Text -> IO (Either LoginError Account)
tryToLogin astate n p = query astate (AttemptLogin n p)

insertMatch :: AcidState TrackerDB -> Match -> IO ()
insertMatch astate = A.update astate . AddMatch

playerMatches :: AcidState TrackerDB -> Text -> GameId -> IO [Match]
playerMatches astate p i = query astate $ PlayersMatches p i

entireSetGet :: AcidState TrackerDB -> IO (IxSet Match)
entireSetGet astate = query astate EntireSet

doRatings :: AcidState TrackerDB -> IO ()
doRatings astate = A.update astate ConductRatings

compileRatings :: AcidState TrackerDB -> IO ()
compileRatings astate = A.update astate RatingsCompile
