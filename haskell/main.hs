module Main where

import JT_Haskell
import System.Environment
import Data.List.Split
import Data.Complex

parse :: String -> [Complex Double]
parse str = map (parseC . (split (oneOf "+-")) . init . init . tail) $ splitOn ", " $ init $ tail str
    where
        parseC [_, _, x, y, z]
            | y == "+" = (:+) (- read x) $ read z
            | y == "-" = (:+) (- read x) (- read z)
        parseC [x, y, z]
                    | y == "+" = (:+) (read x) $ read z
                    | y == "-" = (:+) (read x) (- read z)

main = do 
    args <- getArgs
    let poly = parse $ args !! 0
    let results = jT poly 40 0.00000001
    putStrLn $ show $ results