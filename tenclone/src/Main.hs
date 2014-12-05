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
import           Control.Monad.IO.Class        (liftIO)
import           Data.Acid                     (AcidState)
import           Data.Acid.Local               (openLocalState)
import           Data.ByteString               (ByteString)
import           Data.ByteString.Char8         (pack)
import           Data.Char                     (intToDigit)
import           Data.IxSet                    as I
import           Data.Maybe                    (listToMaybe, mapMaybe)
import           Data.Soku
import           Data.Soku.Accounts
import           Data.Soku.Match
import           Data.Soku.Requests
import           Data.Soku.Requests.Xml
import           Data.Text                     (singleton)
import           Data.Text.Encoding            (decodeUtf8)
import           Data.Time
import           Data.Time.ISO8601
import           Data.Tracker
import           Snap.Core
import           Snap.Http.Server
import           Snap.Util.FileServe
import           Templates.Soku
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)

updateRatings :: AcidState TrackerDB -> IO ()
updateRatings = doRatings

main :: IO ()
main = openLocalState emptyDB >>=
       quickHttpServe . site

unicodeHeader :: Snap ()
unicodeHeader = modifyResponse $
                  setHeader "Content-Type" "text/html; charset=utf-8"

site :: AcidState TrackerDB -> Snap ()
site astate =
    ifTop (indexHandler astate) <|>
    route [ ("api/last_track_record", lastRecordHandler astate)
          , ("api/account", newAccountHandler astate)
          , ("api/track_record", matchRecordHandler astate)
          , ("search", accountHandler astate)
          , ("game/:id/account/:username", accountHandler astate)
          , ("api/testrating", liftIO $ updateRatings astate)
          ] <|>
    (codeReason 404 "Not Found" >>
    writeBS "There is nothing here.") <|>
    dir "static" (serveDirectory ".") -- This will never handle anything as the code is currently written

indexHandler :: AcidState TrackerDB -> Snap ()
indexHandler astate =
    unicodeHeader >>
    indexPage <$> liftIO (playerList astate) >>=
    writeLBS . renderHtml

lastRecordHandler :: AcidState TrackerDB -> Snap ()
lastRecordHandler astate = do
  maybeId <- getParam "game_id"
  maybeName <- getParam "account_name"
  case (,) <$> (decodeUtf8 <$> maybeId >>= parseId) <*> maybeName of
    Nothing           -> codeReason 400 "Bad parameters" >>
                         writeBS "400: Bad parameters"
    Just (gid, pName) -> do
      entireSet <- liftIO $ entireSetGet astate
      case mTime <$> lastTime entireSet of
        Nothing -> codeReason 204 "No record"
        Just t  -> writeBS . pack . formatISO8601 $ t
      where lastTime =
                listToMaybe .
                I.toDescList (I.Proxy :: I.Proxy UTCTime) .
                I.getEQ (gid) .
                I.getEQ (PlayerName $ decodeUtf8 pName)

newAccountHandler :: AcidState TrackerDB -> Snap ()
newAccountHandler astate =
    fmap reqToAcc . parseNewAccount <$> readRequestBody maxBound >>=
    maybe (codeReason 400 "Bad input" >>
           writeBS "400: Bad input")
          tryToRegister
        where tryToRegister accReq =
                  liftIO (registerAccount astate accReq) >>=
                  maybe (writeBS "Success")
                        (\err ->
                         codeReason 400 "Registration Failed" >>
                         writeBS "400: Registration failed: " >>
                         (writeBS . pack) (show err))

matchRecordHandler :: AcidState TrackerDB -> Snap ()
matchRecordHandler astate =
    parseReportLog <$> readRequestBody maxBound >>= \maybeLog ->
    case maybeLog of
      Nothing   -> codeReason 400 "Bad Time" >>
                   writeBS "400: Bad Time"
      Just mLog -> loginAttempt mLog >>= \attempt ->
                   case attempt of
                     Left err   -> codeReason 400 "Login Failure" >>
                                   writeBS "400: Login failed: " >>
                                   (writeBS . pack $ show err)
                     Right acc  -> liftIO $ makeList acc mLog
  where loginAttempt mLog' = liftIO $
          tryToLogin astate (rlName mLog') (rlPass mLog')
        makeList acc (ReportLog _ _ _ mrs _) =
            mapM_ (insertMatch astate) . mapMaybe (requestToMatch acc) $ mrs

accountHandler :: AcidState TrackerDB -> Snap ()
accountHandler astate = do
  unicodeHeader
  username <- getParam "username"
  gameId <- getParam "id"
  case (,) <$> username <*> (gameId >>= parseId . decodeUtf8) of
    Nothing          -> codeReason 400 "No name" >>
                        writeBS "400: No name found"
    Just (name, gId) -> do
      account <- liftIO $ findAccount astate (decodeUtf8 name)
      case account of
        Nothing  -> codeReason 404 "Account not found" >>
                    writeBS "404: That user does not exist."
        Just acc -> do
                     matches <- liftIO $ playerMatches astate (accName acc) gId
                     writeLBS . renderHtml $ playerPage
                                             gId
                                             acc
                                             matches

codeReason :: Int -> ByteString -> Snap ()
codeReason code mess = modifyResponse (setResponseStatus code mess)
