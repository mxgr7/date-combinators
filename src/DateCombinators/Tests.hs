
module DateCombinators.Tests where


import           Chronos hiding (day)
import           Data.Array
import qualified Data.Maybe as Unsafe
import qualified Data.Set as S
import qualified Data.Time.Calendar as C
import           Data.Time.Calendar.WeekDate
import           Data.Tuple.Select
import           DateCombinators
import           Test.Hspec hiding (before, after)
import           Test.QuickCheck
import           Yahp

deriving instance Arbitrary Day

main :: IO ()
main = hspec $ do
  describe "weekdaysBimap" $ do
    it "should be non-empty" $ 
      property $ \x -> not . null . fst . bdBimap . weekdaysBimap x

  describe "dayToWeekday" $ do
      it "should be identical to the `time` version" $ do
       property $ \i -> mod (weekday2 $ C.ModifiedJulianDay i) 7 ===  getDayOfWeek (dayToDayOfWeek (Day $ fromIntegral i))

  describe "DateCombinators" $ do
      let cmp i (a,b, c) =  it ("should satisfy case " <> show i) $ shouldBe (diffBusinessDays cal a b) c
          dd = toDay 2020
          cal = Unsafe.fromJust $ generateBusinessDayBimap $ toDay' <$>  [(2018,1,1),(2020,5,1), (2020,5,5) , (2020,5,3), (2020,5,4), (2022,1,1)]
      zipWithM_ cmp [(1::Int)..]
        [(dd 5 3, dd 5 3    , 0)
        ,(dd 5 2, dd 5 2    , 0)
        ,(dd 5 2, dd 5 6    , 1)
        ,(dd 5 1, dd 5 6    , 1)
        ,(dd 4 30, dd 5 6   , 2)
        ,(dd 4 30, dd 5 6   , 2)
        ,(dd 4 30, dd 5 5   , 2)
        ]

  describe "DateCombinators" $ do
      let cmp i (a,b) =  it ("should satisfy case " <> show i) $ on shouldBe showDay a b
          dd = toDay 2020
          cal = Unsafe.fromJust $ generateBusinessDayBimap $ toDay' <$>  [(2018,1,1),(2020,5,1), (2020,5,5) , (2020,5,3), (2020,5,4), (2022,1,1)]
          bday = businessDay cal
      zipWithM_ cmp [(1::Int)..]
        [(1 `weekday` before (dd april 4)       , dd april 3)
        ,(1 `weekday` after (dd april 4)        , dd april 6)
        ,(1 `weekday` after (dd april 6)        , dd april 7)
        ,(1 `weekday` before (dd april 3)       , dd april 2)
        ,(1 `weekday` onOrBefore (dd april 6)   , dd april 6)
        ,(1 `weekday` onOrBefore (dd april 4)   , dd april 3)
        ,(1 `weekday` onOrBefore (dd april 3)   , dd april 3)
        ,(1 `weekday` onOrAfter (dd april 6)    , dd april 6)
        ,(1 `weekday` onOrAfter (dd april 4)    , dd april 6)
        ,(1 `weekday` onOrAfter (dd april 3)    , dd april 3)
        ,(1 `day` onOrAfter (dd april 3)        , dd april 3)
        ,(1 `day` after (dd april 3)            , dd april 4)
        ,(1 `day` before (dd april 3)           , dd april 2)
        ,(1 `day` onOrBefore (dd april 3)       , dd april 3)
        ,((-1) `day` month (dd april 3)         , dd april 30)
        ,(1 `day` month (dd april 3)            , dd april 1)
        ,(4 `weekday` month (dd april 3)        , dd april 6)
        ,(1 `fri` month (dd april 3)            , dd april 3)
        ,((-1) `fri` month (dd april 3)         , dd april 24)
        ,(1 `wed` month (dd april 3)            , dd april 1)
        ,(5 `wed` month (dd april 3)            , dd april 29)
        ,((-1) `wed` month (dd april 3)         , dd april 29)
        ,((-5) `wed` month (dd april 3)         , dd april 1)
        ,(1 `wed` invert (month $ dd april 3)   , dd april 29)
        ,((-1) `wed` invert (month $ dd april 3), dd april 1)
        ,(1 `bday` (month $ dd 5 3)             , dd 5 2)
        ,(1 `bday` after (1 `bday` (month $ dd 5 3))    , dd 5 6)
        ]

      it "checks bounds" $ shouldThrow (flip seq (pure ()) $ 6 `wed` month (dd april 3)) (== IndexOutOfRange "6")
      it "checks bounds2" $ shouldThrow (flip seq (pure ()) $ (-6) `wed` month (dd april 3)) (== IndexOutOfRange "-6")
      it "checks bounds3" $ shouldThrow (flip seq (pure ()) $ 6 `wed` invert (month $ dd april 3)) (== IndexOutOfRange "6")
      it "checks bounds3" $ shouldThrow (flip seq (pure ()) $ (-6) `wed` invert (month $ dd april 3)) (== IndexOutOfRange "-6")
      it "outside of calendar" $ shouldThrow (flip seq (pure ()) $ 1 `bday` (month $ toDay 2017 april 3)) (\(DayNotInCalendar _) -> True)

  describe "generateBusinessDayBimap" $ do
    it "satisfies a list of properties" $
      let allWeekDays2 f t = filter isWeekend [f..t]
          prop2 hs = maybe (property True) (prop hs . bdBimap) $ generateBusinessDayBimap hs
          prop hs (dToB, bToD) = counterexample (show (holidays, mapping)) $
                                 conjoin [let probs = filter (\(d,bd) -> not $ S.member d holidays ==
                                                               not (isJust $ toBusinessDay bd)) mapping
                                          in counterexample (show probs) $ null probs
                                         ,sortOn snd mapping === sortOn fst mapping
                                         ,numHolidays===length holidays  
                                         ,length dToB===numDays  
                                         ,fromBusinessDay (snd $ bounds bToD)===numBDays  
                                         ,counterexample (show $ length mapping) $  length mapping ===numDays
                                         ]
            where numDays = succ $ on (-) fromEnum (S.findMax holidays) $ S.findMin holidays
                  holidays = S.fromList hs
                  mapping = assocs dToB
                  numBDays = numDays - numHolidays
                  numHolidays = length (filter (not . isJust . toBusinessDay . snd) mapping)
      in property (prop2 . fmap Day) .&&. property (\f t -> prop2 $ allWeekDays2 (Day f) (Day t))

  describe "addBusinessDays" $ do
    let cal = Unsafe.fromJust $ generateBusinessDayBimap $ toDay' <$>  [(2018,1,1),(2020,5,1), (2020,5,5) , (2020,5,3), (2020,5,4), (2022,1,1)]
        cases =                       [ (-1,    (2020, 5, 4), (2020, 4, 30)   )
                                      , (1,     (2020, 5, 4), (2020, 5, 6)    )
                                      , (0,     (2020, 5, 1), (2020, 4, 30)   )
                                      , (0,     (2020, 5, 4), (2020, 5, 2)    )
                                      ]
        cmp i (o,d,res) =  it ("should satisfy case " <> show i <> " for: " <> toS (showDay $ toDay' d)) $ shouldBe
                           (showDay $ addBusinessDays cal o $ toDay' d) $ showDay $ toDay' res
      in zipWithM_ cmp [(1::Int)..] cases

weekday2 :: C.Day -> Int
weekday2 = sel3 . toWeekDate


toDay' :: (Year, Month, DayOfMonth) -> Day
toDay' (y,m,d) = toDay y m d
