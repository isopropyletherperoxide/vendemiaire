module Main (main) where

import Lib

main :: IO ()
main = do 
        let a = dateToFrench 2024 6 6 
        frenchPrint a
