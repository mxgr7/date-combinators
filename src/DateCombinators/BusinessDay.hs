
module DateCombinators.BusinessDay where

import           Chronos
import           Data.Array
import qualified Data.Maybe as Unsafe
import qualified Data.Set as S
import           DateCombinators.Utils
import qualified Prelude as Unsafe
import           Yahp

-- | fractional values represent non-business days between to business days (represented by integers) 
newtype FractionalBusinessDay     = FractionalBusinessDay { fromFractionalBusinessDay :: Double }
  deriving (Eq, Ord, Num, Fractional, Enum, RealFrac, Real)

newtype BusinessDay     = BusinessDay { fromBusinessDay :: Int }
  deriving (Eq, Ord, Num, Enum, Integral, Real, Ix)

toBusinessDay :: FractionalBusinessDay -> Maybe BusinessDay
toBusinessDay fbd = bd <$ guard (fbd == fromIntegral bd)
  where bd = floorBusinessDay fbd

floorBusinessDay :: FractionalBusinessDay -> BusinessDay
floorBusinessDay = floor . fromFractionalBusinessDay

instance Show FractionalBusinessDay where
  show b = "FBday " <> show (fromFractionalBusinessDay b)

instance Show BusinessDay where
  show b = "Bday " <> show (fromBusinessDay b)


data BusinessDayBimap  = UnsafeBusinessDayBimap (Array Day FractionalBusinessDay) (Array BusinessDay Day)

bdBimap (UnsafeBusinessDayBimap a b) = (a,b)

type Holiday            = Day

deriving instance (Ix Holiday)


generateBusinessDays :: [Holiday] -> Maybe (Array Day FractionalBusinessDay)
generateBusinessDays hs' = listArray bounds (gen (toList set) (fst bounds) 0.5) <$ guard (not $ null hs')
  where set = S.fromList hs'
        bounds = (S.findMin set, S.findMax set)
        gen :: [Holiday] -> Day -> FractionalBusinessDay -> [FractionalBusinessDay]
        gen [] _ _                              = []
        gen hs1@(nextHd:hs0) currentDay bday = bd : gen hs (succ currentDay) bd
          where (bd, hs) = first (fromIntegral (floor bday :: Int) +) $ bool (1, hs1) (0.5, hs0) $ nextHd == currentDay


invertBusinessDays :: Array Day FractionalBusinessDay -> Maybe (Array BusinessDay Day)
invertBusinessDays ar = listArray (Unsafe.head bdays, Unsafe.last bdays) days <$ guard (not $ null bdays)  
  where (days, bdays) = unzip $ mapMaybe (traverse toBusinessDay) $ assocs ar


generateBusinessDayBimap :: [Holiday] -> Maybe (BusinessDayBimap)
generateBusinessDayBimap = chain (\x -> UnsafeBusinessDayBimap x <$> invertBusinessDays x) . generateBusinessDays

addBusinessDays :: HasCallStack => BusinessDayBimap -> Int -> Day -> Day
addBusinessDays bi n day | (days, bdays) <- bdBimap bi 
  = lookupMsg (toS . show) bdays . (+ fromIntegral n) . floorBusinessDay $ lookupMsg showDay days day

-- counts business days between a (inclusive) and b (exclusive)
diffBusinessDays :: BusinessDayBimap -> Day -> Day -> Int
diffBusinessDays bim a b = on (-) (ceiling . fromFractionalBusinessDay . lookupMsg (toS . show) (fst $ bdBimap bim)) b a


lookupMsg :: (HasCallStack, Ix t) => (t -> Text) -> Array t a -> t -> a
lookupMsg sh a i | inRange b i =  a ! i
                 | True        = throw $ DayNotInCalendar $ sh i <> " not in range (" <> sh l <> ", " <> sh u <> ")"
  where b@(l,u) = bounds a

data BDException = DayNotInCalendar Text
  deriving (Generic, Eq, Show)

instance Exception BDException

weekdaysBimap :: Day -> Day -> BusinessDayBimap
weekdaysBimap from to = Unsafe.fromJust
  $ generateBusinessDayBimap $ filter isWeekend [from .. max (addDays 14 from) to]

