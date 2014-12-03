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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module Data.Soku ( Character(..)
                 , GameId(..)
                 , parseId
                 , parseCharacter
                 , showText
                 , idToInt
                 ) where

import           Data.Data
import           Data.SafeCopy (base, deriveSafeCopy)
import           Data.Text     (Text, pack)

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
               | Utsuho
               | Suwako
               | Reisen
               | Aya
               | Komachi
               | Iku
               | Tenshi
                 deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Character)

showText :: Show a => a -> Text
showText = pack . show

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

-- The tenco game id numbers
idToInt :: GameId -> Int
idToInt SWR    = 1
idToInt Soku   = 2
idToInt HMDemo = 3
idToInt HM     = 4

parseCharacter :: Text -> Maybe Character
parseCharacter t = case t of
                     "0"  -> Just Reimu
                     "1"  -> Just Marisa
                     "2"  -> Just Sakuya
                     "4"  -> Just Patchouli
                     "5"  -> Just Youmu
                     "7"  -> Just Yuyuko
                     "8"  -> Just Yukari
                     "9"  -> Just Suika
                     "13" -> Just Iku
                     "14" -> Just Tenshi
                     "17" -> Just Meiling
                     "18" -> Just Utsuho
                     _    -> Just Sanae -- is a good girl (replace with Nothing when
                                        -- character ids are filled out completely
