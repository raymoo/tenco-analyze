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
module Templates.Soku(
                       indexPage
                     , playerPage
                     ) where

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Internal (textValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Text (Text)
import Data.Text as T (append, pack)
import Data.Soku.Match
import Data.Time.ISO8601
import Data.Soku

-- | Fix to Text
wombo :: Text -> Text
wombo = Prelude.id

-- | Index page done in blaze because I am lazy
indexPage :: [Text] -> Html
indexPage ps = 
    docTypeHtml $ do
      H.head $
        H.title "Welcome to Tenclone!"
      body $ do
        toHtml $ wombo "Welcome to Tenclone!"
        br
        br
        toHtml $ wombo "Account search:"
        br
        H.form ! action "search" $ do
          toHtml $ wombo "Username: "
          br
          input ! type_ "text" ! name "username"
          br
          br
          toHtml $ wombo "Game ID: "
          br
          input ! type_ "text" ! name "id"
          br
          input ! type_ "submit" ! value "Go"
        br
        br
        toHtml $ wombo "Players:"
        br
        playerList ps

playerList :: [Text] -> Html
playerList ps = table $ mapM_ playerRow ps
    where playerRow pl =
              tr $ do
                td $ toHtml pl
                mapM_ (td . profileLink pl) ["1","2","3","4"]

type GameID = Text
type Username = Text

profileLink :: Username -> GameID -> Html
profileLink pName gid = a ! href (textValue profAddress) $ toHtml gid
  where profAddress = "game/" `T.append` gid `T.append` "/account/" `T.append` pName

playerPage :: GameID -> Username -> [Match] -> Html
playerPage gid uname ms = 
    docTypeHtml $ do
      H.head $
        H.title $ toHtml $ uname `append` "'s profile"
      body $ do
        h1 $ toHtml $ uname `append` "'s stats"
        br
        br
        table $ do 
               tr $ do
                     td $ toHtml $ wombo "Time"
                     td $ toHtml $ wombo "Player"
                     td $ toHtml $ wombo "Score"
                     td $ toHtml $ wombo "Opponent"
               mapM_ gameRow ms
    where gameRow (Match t 
                         g
                         _
                         pHandle
                         oName
                         oHandle
                         matched
                         won
                         pChar
                         oChar
                         score) =
              tr $ do
                td $ toHtml $ formatISO8601 t
                td $ maybeBold won $ toHtml $ pHandle `T.append`
                                              " (" `T.append`
                                              showText pChar `T.append`
                                              ") "
                td $ toHtml $ scoreText score
                td $ maybeBold (not won) $ toHtml $ oName `T.append`
                                                    " (" `T.append`
                                                    showText oChar `T.append`
                                                    ") "
          scoreText (score1, score2) = (T.pack . show) score1 `T.append`
                                       " - " `T.append`
                                       (T.pack . show) score2

maybeBold :: Bool -> Html -> Html
maybeBold True h  = b h
maybeBold False h = h
