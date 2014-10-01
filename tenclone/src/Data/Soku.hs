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

{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, OverloadedStrings #-}
module Data.Soku ( Character(..)
                 , GameId(..)
                 , parseId
                 , parseCharacter
                 ) where

import Data.Data
import Data.SafeCopy (deriveSafeCopy, base)
import Data.Text (Text)

-- | Enumerates the different characters in hisoutensoku
data Character = Suika
               | Yukari
               | Yuyuko
               | Remilia
               | Youmu
               | Patchouli
               | Alice
               | Sakuya
               | Marisa
               | Reimu
               | Sanae
               | Cirno
               | Meiling
               | Utshuho
               | Suwako
               | Reisen
               | Aya
               | Komachi
               | Iku
               | Tenshi
                 deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Character)

-- | Enumerates the different possible games
data GameId = SWR
            | Soku
            | HMDemo
            | HM
              deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''GameId)

parseId :: Text -> Maybe GameId
parseId x = case x of
              "1" -> Just SWR
              "2" -> Just Soku
              "3" -> Just HMDemo
              "4" -> Just HM
              _   -> Nothing

parseCharacter :: Text -> Maybe Character
parseCharacter = const $ Just Sanae -- Is a good girl
