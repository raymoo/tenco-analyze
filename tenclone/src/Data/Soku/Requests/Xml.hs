module Data.Soku.Requests.Xml (
                                parseNewAccount
                              , parseReportLog
                              ) where

import Text.XML.Light
import Text.XML.Light.Lexer (XmlSource)
import Data.Soku.Requests
import Data.Soku
import Data.Maybe (listToMaybe, mapMaybe)
import Control.Applicative
import Data.Text as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.ByteString.Lazy (ByteString)

parseNewAccount :: ByteString -> Maybe NewAccountReq
parseNewAccount = (xmlToNewAccount =<<) . parseXMLDoc . decodeUtf8

parseReportLog :: ByteString -> Maybe ReportLog
parseReportLog = (xmlToReportLog =<<) . parseXMLDoc . decodeUtf8

rootNamePossibly :: String -> Element -> Maybe Element
rootNamePossibly t xml
    | qName (elName xml) == t = Just xml
    | otherwise               = Nothing

childTextPossibly :: String -> Element -> Maybe String
childTextPossibly text xml = strContent <$> listToMaybe (findChildren (unqual text) xml)

xmlToNewAccount :: Element -> Maybe NewAccountReq
xmlToNewAccount xml = rootNamePossibly "account" xml *>
                      findAccData
  where
    findAccData = liftA3 NewAccountReq
                  (T.pack <$> childTextPossibly "name" xml)
                  (T.pack <$> childTextPossibly "password" xml)
                  (T.pack <$> childTextPossibly "mail_address" xml)

data Login = Login  { logName :: T.Text
                    , logPass :: T.Text
                    } 

data Game = Game { gid :: T.Text
                 , matches :: [MatchResult]
                 }
          deriving (Show)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- | Parses one track record
xmlToRecord :: T.Text -> Element -> Maybe MatchResult
xmlToRecord theid xml = rootNamePossibly "trackrecord" xml >>
                        MatchResult <$>
                        childTextPossibly "timestamp" xml <*>
                        pure theid <*>
                        textPos "p1name" <*>
                        (textPos "p1type" >>= parseCharacter) <*>
                        (childTextPossibly "p1point" xml >>= maybeRead) <*>
                        textPos "p2name" <*>
                        (textPos "p2type" >>= parseCharacter) <*>
                        (childTextPossibly "p2point" xml >>= maybeRead)
    where textPos name = T.pack <$> childTextPossibly name xml

-- | Parses <game> tag from xml
xmlToGame :: Element -> Maybe Game
xmlToGame xml = rootNamePossibly "game" xml *>
                liftA2 Game
                       (T.pack <$> childTextPossibly "id" xml)
                       ((mapMaybe . xmlToRecord) <$> theid <*> pure (findChildren (unqual "trackrecord") xml))
    where theid = T.pack <$> childTextPossibly "id" xml

xmlToReportLog :: Element -> Maybe ReportLog
xmlToReportLog xml = rootNamePossibly "trackrecordPosting" xml >>
                     childPossibly "account" >>= findAccData >>= \account ->
                     childPossibly "game" >>= xmlToGame >>= \game ->
                     combine account game
    where combine (Login lName lPass) (Game gameId matches) =
              ReportLog lName lPass <$> parseId gameId <*> pure matches <*> pure False
          findAccData aXml =
              liftA2 Login
                     (T.pack <$> childTextPossibly "name" aXml)
                     (T.pack <$> childTextPossibly "password" aXml) 
          childPossibly text = listToMaybe $ findChildren (unqual text) xml

