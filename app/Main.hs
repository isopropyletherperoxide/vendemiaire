module Main (main) where

import Lib
import Options.Applicative
import Data.Time (parseTimeM, defaultTimeLocale, toGregorian)
import Data.Time.Calendar (Day)
import System.Exit (exitFailure, ExitCode (ExitFailure))
import Data.Time.Clock

data Sample = Sample
  { date      :: Maybe String
  , french      :: Bool }

sample :: Parser Sample
sample = Sample
      <$> optional (strOption 
          ( long "date"
         <> metavar "TARGET"
         <> help "Target for the greeting" ) )
      <*> switch
          ( long "french"
         <> short 'f'
         <> help "Whether to be French" )

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

greet :: Sample -> IO ()
greet (Sample (Just h) False) = do 
        let haha = dateFromArg h
        case haha of 
          Just a -> putStrLn $ show a
          Nothing -> exitFailure
greet (Sample Nothing False) = do
        today <- todaysDate
        maybe (putStrLn "Error!") print (properDateToFrench (Just today)) 
greet _ = return ()

dateFromArg = properDateToFrench . properDate . dateFromArg1

todaysDate :: IO (Integer,Int,Int) -- :: (year,month,day)
todaysDate = getCurrentTime >>= return . toGregorian . utctDay

properDateToFrench :: Maybe (Integer, Int, Int) -> Maybe (Int, RepublicanMonth, Int)
properDateToFrench (Just (a, b, c)) = return $ dateToFrench (fromInteger a) b c
properDateToFrench Nothing = Nothing


properDate :: Maybe Day -> Maybe (Integer, Int, Int)
properDate (Just a) = return $ toGregorian a
properDate Nothing = Nothing

dateFromArg1 :: String -> Maybe Day
dateFromArg1 a = parseTimeM False defaultTimeLocale "%Y-%m-%d" a :: Maybe Day

