{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable #-}

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
                    ) where

import Data.Soku.Accounts
import Data.Soku.Match
import Data.Data
import Data.SafeCopy
import Data.Acid
import Data.Acid as A
import Data.IxSet
import Data.IxSet as I
import Data.Text (Text)
import Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import Data.Time (UTCTime)

-- | All the site data
data TrackerDB = TrackerDB
    { accountDB :: AccountList
    , matchDB   :: IxSet Match
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
attemptLogin :: Text -> Text -> Query TrackerDB (Maybe LoginError)
attemptLogin n p = tryLogin . accountDB <$> ask <*> pure n <*> pure p

-- | Return a list of players
listOfPlayers :: Query TrackerDB [Text]
listOfPlayers = getKeys . accountDB <$> ask
    where getKeys (AccountList as) = Map.keys as

-- | Inserts a match into the database
addMatch :: Match -> Update TrackerDB ()
addMatch m = modify $ modifyMatch (I.insert m)
    where modifyMatch f tracker =
              tracker { matchDB = f $ matchDB tracker }

-- | Gets the matches a player reported.
playersMatches :: Text -> Query TrackerDB [Match]
playersMatches name = I.toDescList (I.Proxy :: I.Proxy UTCTime) . 
                      getEQ (PlayerName name) . matchDB <$> ask

entireSet :: Query TrackerDB (IxSet Match)
entireSet = matchDB <$> ask

$(makeAcidic ''TrackerDB ['regUpdate, 'queryAccount, 'listOfPlayers, 'attemptLogin, 'addMatch, 'playersMatches, 'entireSet])

registerAccount :: AcidState TrackerDB -> Account -> IO (Maybe RegError)
registerAccount astate acc = A.update astate (RegUpdate acc)

findAccount :: AcidState TrackerDB -> Text -> IO (Maybe Account)
findAccount astate name = query astate (QueryAccount name)

playerList :: AcidState TrackerDB -> IO [Text]
playerList astate = query astate ListOfPlayers

tryToLogin :: AcidState TrackerDB -> Text -> Text -> IO (Maybe LoginError)
tryToLogin astate n p = query astate (AttemptLogin n p)

insertMatch :: AcidState TrackerDB -> Match -> IO ()
insertMatch astate = A.update astate . AddMatch

playerMatches :: AcidState TrackerDB -> Text -> IO [Match]
playerMatches astate = query astate . PlayersMatches

entireSetGet :: AcidState TrackerDB -> IO (IxSet Match)
entireSetGet astate = query astate EntireSet

