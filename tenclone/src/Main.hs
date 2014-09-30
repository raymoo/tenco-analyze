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

{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Data.Soku.Requests.Xml
import           Data.Soku.Accounts
import           Data.Acid (AcidState)
import           Data.Acid.Local (openLocalState)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString.Char8 (pack)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Traversable as Trav (mapM)
import           Control.Monad (join)
import           Templates.Soku
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)


main :: IO ()
main = openLocalState emptyAccList >>=
       quickHttpServe . site

site :: AcidState AccountList -> Snap ()
site astate =
    ifTop (writeLBS $ renderHtml indexPage) <|>
    route [ ("api/last_track_record", writeBS "2010-09-27T22:52:00+00:00")
          , ("api/account", newAccountHandler astate)
          , ("api/track_record", matchRecordHandler)
          , ("search", accountHandler astate)
          , ("game/:id/account/:username", accountHandler astate)
          ] <|>
    writeBS "There is nothing here." <|>
    dir "static" (serveDirectory ".") -- This will never handle anything as the code is currently written

newAccountHandler :: AcidState AccountList -> Snap ()
newAccountHandler astate = 
    parseNewAccount <$> readRequestBody maxBound >>= 
    maybe ((modifyResponse $ setResponseStatus 400 "Bad input") >> 
           writeBS "400: Bad input")
          tryToRegister
        where tryToRegister accReq =
                  liftIO (registerAccount astate accReq) >>=
                  maybe (writeBS "Success") 
                        (\err ->
                         modifyResponse (setResponseStatus 400 "Registration Failed") >>
                         writeBS "400: Registration failed: " >>
                         (writeBS . pack) (show err))
                                     

matchRecordHandler :: Snap ()
matchRecordHandler =
    writeBS "Your request body: \n" >>
    readRequestBody maxBound >>= writeLBS >>
    writeBS "\nThis is a test. No reports were recorded."

accountHandler :: AcidState AccountList -> Snap ()
accountHandler astate = 
    getParam "username" >>= \username ->
    Trav.mapM (liftIO . findAccount astate . decodeUtf8) username >>=
    maybe (modifyResponse (setResponseStatus 404 "Nonexistent Account") >>
           Trav.mapM writeBS username >> writeBS " is not registered.")
          ((writeBS "Found account: " >>) . writeText . accName) . join

