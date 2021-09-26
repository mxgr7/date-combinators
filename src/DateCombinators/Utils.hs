
module DateCombinators.Utils where

import           Chronos
import qualified Torsor as T
import           Yahp

addDays :: Int -> Day -> Day
addDays = T.add

showDay :: Day -> Text
showDay = encode_Ymd (Just '.') . dayToDate

deriving instance Num DayOfMonth
instance Num Month where
  (+) = lm2 (+)
  (*) = lm2 (*)
  negate = lm negate
  abs = lm abs
  signum = lm signum
  fromInteger = Month . pred . fromInteger -- The reason for this manual instance !! 


lm2 f (Month a) (Month b) = Month $ f a b
lm f (Month a) = Month $ f a

deriving instance Num Year

dayToDayOfWeek :: Day -> DayOfWeek
dayToDayOfWeek = timeToDayOfWeek . dayToTimeMidnight

isWeekend :: Day -> Bool
isWeekend = (\x -> x == sunday || x == saturday) . dayToDayOfWeek

toDay :: Year -> Month -> DayOfMonth -> Day
toDay = fmap3 dateToDay Date
