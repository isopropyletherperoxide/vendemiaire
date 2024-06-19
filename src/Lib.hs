module Lib
    (
    dayToFrench,
    frenchDayToMonth,
    getFrenchMonthFromDay,
    getFrenchMonth,
    yearToFrench,
    isLeap,
    dateToFrench,
    dayToFrenchLeap,
    dayOfYear,
    monthDay, RepublicanMonth(..),
    frenchPrint, frenchDateToString
    ) where
import Data.List (foldl')

type Year = [Int]

-- | nTh number of the Gregorian Year (1 - 366)
type GregorianDay = Int

-- | nTh month of the Republican Year (0-12) 
type RepublicanMonthNum = Int

type GregorianYear = Int

type GregorianMonth = Int

type RepublicanDay = Int

type GregorianMonthDay = Int

data RepublicanMonth =  Vendémiaire
            | Brumaire
            | Frimaire
            | Nivôse
            | Pluviôse
            | Ventôse
            | Germinal
            | Floréal
            | Prairial
            | Messidor
            | Thermidor
            | Fructidor
            | Sansculottides
            deriving (Enum, Ord, Eq, Show)

-- I/O

frenchPrint :: (Int, RepublicanMonth, Int) -> String
frenchPrint (year, month, day) = "\ESC[0;34m" ++ show year ++ " " ++ "\ESC[0;0m" ++ show month ++ " " ++ "\ESC[0;31m" ++ show day ++ "\ESC[0;0m"

frenchDateToString :: (Int, RepublicanMonth, Int) -> String 
frenchDateToString (year, month, day) = show year ++ " " ++ show month ++ " " ++ show day 


-- Date Handling:

dayToFrench :: GregorianDay -> Int
dayToFrench a = mod (a + 100) 365 + 1

monthDay :: Int -> Int
monthDay a = ((a - 1) `mod` 30) + 1

dayToFrenchLeap :: GregorianDay -> Int
dayToFrenchLeap a = mod (a + 100) 366 + 1

dateToFrench :: GregorianYear -> GregorianMonth -> GregorianMonthDay -> (Int, RepublicanMonth, Int)
dateToFrench year month day = (yearToFrench year day', getFrenchMonthFromDay frenchDay, monthDay frenchDay)
        where frenchDay = dayToFrench_ year day'
              day' = dayOfYear year month day

leapyear :: Year
leapyear = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]

normalyear :: Year
normalyear = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]


dayOfYear :: Int -> Int -> Int -> Int
dayOfYear year month day | isLeap year = foldl' (+) 0 (take (month - 1) leapyear) + day
  | otherwise = foldl' (+) 0 (take (month - 1) normalyear) + day


dayToFrench_ :: Int -> Int -> Int
dayToFrench_ year day | isLeap year = dayToFrenchLeap day
                         | otherwise = dayToFrench day

frenchDayToMonth :: Int -> Int
frenchDayToMonth a = floor ((fromIntegral a / 30) :: Double)

yearToFrench :: Int -> Int -> Int
yearToFrench year civil_day | isLeap year && civil_day < 266 = year - 1792
  | civil_day < 266 && not (isLeap year) = year - 1792
  | otherwise = year - 1791

isLeap :: Int -> Bool
isLeap a = (a `mod` 4 == 0) && (a `mod` 100 /= 0) || (a `mod` 400 == 0)

getFrenchMonthFromDay :: RepublicanDay -> RepublicanMonth
getFrenchMonthFromDay a | a > 360 = Sansculottides
                        | otherwise  = toEnum $ frenchDayToMonth a

getFrenchMonth :: RepublicanMonthNum -> RepublicanMonth
getFrenchMonth = toEnum

