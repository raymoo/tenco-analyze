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
    ifTop (writeBS "Server not operational yet!") <|>
    route [ ("foo", writeBS "bar")
          , ("game/:id/account/:username", accountHandler)
          ] <|>
    writeBS "There is nothing here." <|>
    dir "static" (serveDirectory ".") -- This will never handle anything as the code is currently written

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
