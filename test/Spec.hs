import Lib
main :: IO ()
main = do 
        let a = dateToFrench 2024 6 5
        let b = dateToFrench 2076 9 21 
        print "TEST A"
        frenchPrint a
        print (a == (232, Prairial, 18))
        print "TEST B"
        frenchPrint b
        print (b == (284,Sansculottides,6))
