{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Rating.Glicko(
                           Rating(..)
                         , defRating
                         , WLT(..)
                         , Outcome
                         , advanceRating
                         ) where

import Data.Data
import Data.SafeCopy

-- | Glicko rating
data Rating =
  Rating { rScore :: Int
         , rDev   :: Int -- deviation
         , rTime  :: Int -- rating periods since last competition
                         -- rTime = 1 if the player played last rating period
         }
  deriving (Show, Eq, Ord, Data, Typeable)

deriveSafeCopy 0 'base ''Rating

-- | The starting rating - 1500 with 350 deviation
defRating :: Rating
defRating = Rating defScore maxDev 0
  where defScore = 1500


maxDev :: Integral a => a
maxDev = 350

-- | Use this to prevent stale ratings
minDev :: Integral a => a
minDev = 30


-- | Constant that decides how quickly deviation should rise with time.
-- A good value is maxDev / n, where n is the expected number of rating
-- periods without playing it takes to go from perfectly-rated to completely
-- unsure.
c_sq :: Floating a => a
c_sq = fromInteger (maxDev :: Integer) / expectedN
  where expectedN = -- ^ Rating period is one hour, expected time is two months
          24 * 30 * 2

-- | Increases deviation 
advanceDeviation :: Rating -> Rating
advanceDeviation rating@Rating { rDev = dev, rTime = t } =
  let newDev = min minDev . max maxDev $ round unBoundedDev
      unBoundedDev = sqrt (fromIntegral dev^2 + c_sq * fromIntegral t) :: Double
  in rating { rDev = newDev }


-- | Win Loss Tie
data WLT = W
         | L
         | T

-- | converts 'WLT' to the numbers specified in
-- http://www.glicko.net/glicko/glicko.pdf as "outcome"
wltNum :: Floating a => WLT -> a
wltNum W = 1
wltNum L = 0
wltNum T = 0.5

-- | Opponent's rating and result of the match
type Outcome = (Rating, WLT)

sumOn :: Num b => (a -> b) -> [a] -> b
sumOn f = sum . map f

newRating :: [Outcome] -> Rating -> Rating
newRating os rating = Rating r' rd' 0
  where r = fromIntegral $ rScore rating
        
        rd = fromIntegral $ rDev rating

        q :: Double
        q = 0.0057565 -- ln 10 / 400
        
        g dev = 1 / sqrt (1 + 3 * q^2 * dev^2 / pi^2)
        
        -- | Expected outcome
        e r_j rd_j = 1 / (1 + 10 ** (-1 * g rd_j * (r - r_j) / 400))
        
        ratings = map fst os
        
        d_sq = 1 / (q^2 * sumOn d_step ratings)
          where d_step (Rating r_jn rd_jn _) =
                  let rd_j = fromIntegral rd_jn
                      r_j = fromIntegral r_jn
                  in g rd_j ^ 2 * e r_j rd_j * (1 - e r_j rd_j)
                  
        r' :: Int
        r' = round $ r + q / (1 / rd^2 + 1 / d_sq) * sumOn r'_step os
          where r'_step (Rating r_jn rd_jn _, wlt) =
                  let s_j = wltNum wlt
                      r_j = fromIntegral r_jn
                      rd_j = fromIntegral rd_jn
                  in g rd_j * (s_j - e r_j rd_j)
                     
        rd' :: Int
        rd' = round $ sqrt $ 1 / (1 / rd ^ 2 + 1 / d_sq)

-- | Given a rating and matches in the last rating period, advances the
-- rating
advanceRating :: [Outcome] -> Rating -> Rating
advanceRating [] rating = advanceTime rating
advanceRating os rating = newRating os .
                          advanceDeviation .
                          advanceTime $
                          rating

advanceTime :: Rating -> Rating
advanceTime r@Rating { rTime = t } =
  r { rTime = t + 1 }
