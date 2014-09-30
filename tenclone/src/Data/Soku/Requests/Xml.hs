module Data.Soku.Requests.Xml (
                                parseNewAccount
                              , parseReportLog
                              ) where

import Text.XML.Light
import Text.XML.Light.Lexer (XmlSource)
import Data.Soku.Requests
import Data.Maybe (listToMaybe)
import Control.Applicative ((<$>), liftA3)
import Data.Text as T

parseNewAccount :: XmlSource s => s -> Maybe NewAccountReq
parseNewAccount = (xmlToNewAccount =<<) . parseXMLDoc

parseReportLog :: XmlSource s => s -> Maybe ReportLog
parseReportLog = (xmlToReportLog =<<) . parseXMLDoc

xmlToNewAccount :: Element -> Maybe NewAccountReq
xmlToNewAccount xml
  | qName (elName xml) == "account" = findAccData
  | otherwise                       = Nothing
  where
    findAccData = liftA3 NewAccountReq
                  (T.pack . strContent <$> listToMaybe (findChildren (unqual "name") xml))
                  (T.pack . strContent <$> listToMaybe (findChildren (unqual "password") xml))
                  (T.pack . strContent <$> listToMaybe (findChildren (unqual "mail_address") xml))

xmlToReportLog :: Element -> Maybe ReportLog
xmlToReportLog = undefined
