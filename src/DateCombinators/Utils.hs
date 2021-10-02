
module DateCombinators.Utils where

import           Chronos
import qualified Data.Time.Calendar as D
import qualified Torsor as T
import           Yahp
import           Prelude as Unsafe

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


data DateCombinatorsException = IndexOutOfRange String
                              | InfinitDate
                              | ZeroIndex
                              | DayNotInCalendar Text
                              | HolidayCalenderNotAvailable Text
                              | ParseError String
                              | DynamicTypeMismatch String
                              deriving (Generic, Eq)

instance Show  DateCombinatorsException where
  show = \case
    IndexOutOfRange s                -> "IndexOutOfRange: " <> s
    InfinitDate                      -> "InfinitDate"
    ZeroIndex                        -> "ZeroIndex"
    DayNotInCalendar s               -> "DayNotInCalendar: " <> toS s
    ParseError s                     -> "ParseError: " <> s
    DynamicTypeMismatch s            -> "DynamicTypeMismatch: " <> s
    HolidayCalenderNotAvailable s    -> "HolidayCalenderNotAvailable: " <> toS s




    
  

instance Exception DateCombinatorsException

toBaseDay :: Day -> D.Day
toBaseDay = D.ModifiedJulianDay . fromIntegral . getDay

fromBaseDay :: D.Day -> Day
fromBaseDay x | i == fromIntegral y     = Day y
              | True                    = Unsafe.error $ "date " <> show x <> "outside of int range"
  where i = D.toModifiedJulianDay x :: Integer
        y = fromIntegral i :: Int
