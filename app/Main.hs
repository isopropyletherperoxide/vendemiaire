{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
module Main (main) where

import Data.Time (defaultTimeLocale, parseTimeM, toGregorian)
import Data.Time.Calendar (Day)
import Data.Time.Clock
import Lib
import Lib (frenchDateToString)
import Options.Applicative
import System.Exit (ExitCode (ExitFailure), exitFailure)

data Sample = Sample
  { date :: Maybe String,
    french :: Bool
  }

sample :: Parser Sample
sample =
  Sample
    <$> optional
      ( strOption
          ( long "date"
              <> metavar "TARGET"
              <> help "Target for the greeting"
          )
      )
    <*> switch
      ( long "french"
          <> short 'f'
          <> help "Whether to be French"
      )

main :: IO ()
main = greet =<< execParser opts
  where
    opts =
      info
        (sample <**> helper)
        ( fullDesc
            <> progDesc "Print a greeting for TARGET"
            <> header "vendemiare - a gregorian to french republican date converter"
        )

greet :: Sample -> IO ()
greet (Sample inDate frenchmode) = do
  today <- case inDate of
    Just a -> maybe exitFailure return (dateFromArg a)
    Nothing -> do
      toop <- todaysDate
      maybe exitFailure return (properDateToFrench (Just toop))
  case frenchmode of
    True -> putStrLn $ frenchPrint today
    False -> putStrLn $ frenchDateToString today

dateFromArg :: String -> Maybe (Int, RepublicanMonth, Int)
dateFromArg = properDateToFrench . properDate . dateFromArg1

todaysDate :: IO (Integer, Int, Int) -- :: (year,month,day)
todaysDate = getCurrentTime >>= return . toGregorian . utctDay

properDateToFrench :: Maybe (Integer, Int, Int) -> Maybe (Int, RepublicanMonth, Int)
properDateToFrench (Just (a, b, c)) = return $ dateToFrench (fromInteger a) b c
properDateToFrench Nothing = Nothing

properDate :: Maybe Day -> Maybe (Integer, Int, Int)
properDate (Just a) = return $ toGregorian a
properDate Nothing = Nothing

dateFromArg1 :: String -> Maybe Day
dateFromArg1 a = parseTimeM False defaultTimeLocale "%Y-%m-%d" a :: Maybe Day
