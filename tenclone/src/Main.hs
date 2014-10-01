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

import           Data.Maybe (mapMaybe)
import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Data.Soku.Requests.Xml
import           Data.Soku.Requests
import           Data.Soku.Match
import           Data.Tracker
import           Data.Soku.Accounts
import           Data.Acid (AcidState)
import           Data.Acid.Local (openLocalState)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString.Char8 (pack)
import           Data.Text.Encoding (decodeUtf8)
import           Control.Monad (join)
import           Templates.Soku
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)


main :: IO ()
main = openLocalState emptyDB >>=
       quickHttpServe . site

site :: AcidState TrackerDB -> Snap ()
site astate =
    ifTop (indexHandler astate) <|>
    route [ ("api/last_track_record", writeBS "2010-09-27T22:52:00+00:00")
          , ("api/account", newAccountHandler astate)
          , ("api/track_record", matchRecordHandler astate)
          , ("search", accountHandler astate)
          , ("game/:id/account/:username", accountHandler astate)
          ] <|>
    writeBS "There is nothing here." <|>
    dir "static" (serveDirectory ".") -- This will never handle anything as the code is currently written

indexHandler :: AcidState TrackerDB -> Snap ()
indexHandler astate = 
    indexPage <$> liftIO (playerList astate) >>=
    writeLBS . renderHtml

newAccountHandler :: AcidState TrackerDB -> Snap ()
newAccountHandler astate = 
    parseNewAccount <$> readRequestBody maxBound >>= 
    maybe (modifyResponse (setResponseStatus 400 "Bad input") >> 
           writeBS "400: Bad input")
          tryToRegister
        where tryToRegister accReq =
                  liftIO (registerAccount astate accReq) >>=
                  maybe (writeBS "Success") 
                        (\err ->
                         modifyResponse (setResponseStatus 400 "Registration Failed") >>
                         writeBS "400: Registration failed: " >>
                         (writeBS . pack) (show err))
                                     

matchRecordHandler :: AcidState TrackerDB -> Snap ()
matchRecordHandler astate =
    parseReportLog <$> readRequestBody maxBound >>= \maybeLog ->
    case maybeLog of
      Nothing   -> modifyResponse (setResponseStatus 400 "Bad Time") >>
                   writeBS "400: Bad Time"
      Just mLog -> loginAttempt mLog >>= \attempt ->
                   case attempt of
                     Just err -> modifyResponse (setResponseStatus 400 "Login Failure") >>
                                 writeBS "400: Login failed: " >>
                                 (writeBS . pack $ show err)
                     Nothing  -> liftIO $ makeList mLog
  where loginAttempt mLog' = liftIO $ tryToLogin astate (rlName mLog') (rlPass mLog')
        makeList (ReportLog n _ gid mrs _) =
            mapM_ (insertMatch astate) . mapMaybe (requestToMatch n) $ mrs

accountHandler :: AcidState TrackerDB -> Snap ()
accountHandler astate = do
  username <- getParam "username"
  gameId <- getParam "id"
  case (,) <$> username <*> gameId of
    Nothing          -> modifyResponse (setResponseStatus 400 "No name") >>
                        writeBS "400: No name found"
    Just (name, gId) -> do
      account <- liftIO $ findAccount astate (decodeUtf8 name)
      case account of
        Nothing  -> modifyResponse (setResponseStatus 404 "Account not found") >>
                    writeBS "404: That user does not exist."
        Just acc -> do
                     matches <- liftIO $ playerMatches astate (accName acc)
                     writeLBS . renderHtml $ playerPage
                                             (decodeUtf8 gId)
                                             (decodeUtf8 name)
                                             matches
