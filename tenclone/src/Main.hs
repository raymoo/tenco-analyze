{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "Basic server that doesn't do anything interesting.") <|>
    route [ ("api/last_track_record", writeBS "2010-09-27T22:52:00+00:00")
          , ("api/account", newAccountHandler)
          , ("game/:id/account/:username", accountHandler)
          ] <|>
    writeBS "There is nothing here." <|>
    dir "static" (serveDirectory ".") -- This will never handle anything as the code is currently written

newAccountHandler :: Snap ()
newAccountHandler = 
    writeBS "Your request body: \n" >>
    readRequestBody maxBound >>= writeLBS >>
    writeBS "\nThis is a test. No accounts were created."
                    

accountHandler :: Snap ()
accountHandler = do
    gId <- getParam "id"
    username <- getParam "username"
    let maybeHolder = makeAccountPlaceholder <$> gId <*> username
    maybe (writeBS "bad parameters") id maybeHolder
        where makeAccountPlaceholder gId aName = 
                  writeBS "In the future, this page will display the stats of " >>
                  writeBS aName >>
                  writeBS " for the game of id " >>
                  writeBS gId
