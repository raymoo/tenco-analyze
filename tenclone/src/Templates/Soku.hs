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
module Templates.Soku where

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Text (Text)

-- | Fix to Text
wombo :: Text -> Text
wombo = Prelude.id

-- | Index page done in blaze because I am lazy

indexPage :: Html
indexPage = 
    docTypeHtml $ do
      H.head $ do
        H.title "Welcome to Tenclone!"
      body $ do
        toHtml $ wombo "Welcome to Tenclone!"
        br
        br
        toHtml $ wombo "Account search:"
        br
        H.form ! action "search" $ do
          toHtml $ wombo "Username: "
          input ! type_ "text" ! name "username"
          br
          toHtml $ wombo "Game ID: "
          input ! type_ "text" ! name "id"
          input ! type_ "submit" ! value "Search"
