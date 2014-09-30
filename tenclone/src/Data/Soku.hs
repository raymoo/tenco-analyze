module Data.Soku ( Character(..)
                 , GameId(..)
                 ) where

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
                 deriving (Eq, Ord, Show)

-- | Enumerates the different possible games
data GameId = SWR
            | Soku
            | HMDemo
            | HM
              deriving (Eq, Ord, Show)
