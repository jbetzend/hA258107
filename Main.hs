module Main where

import System.Environment          (getArgs)
import Data.List                   (findIndex)
import Data.Digits                 (digits, unDigits)
import Control.Parallel.Strategies (parListChunk, using, rseq)

convertBase :: Integral a => a -> a -> [a] -> [a]
convertBase from to = digits to . unDigits from

valid :: Integral a => a -> a -> [a] -> Bool
valid from to = (all (\d -> (d < 2))) . (convertBase from to)

check :: Integer -> Bool
check n = and [valid 6 b m | b <- [3,4,5]]
  where
    m = (convertBase 10 2) (digits 10 n)

main :: IO ()
main = do [arg] <- getArgs
          loop (read arg)
  where
    loop :: Integer ->  IO ()
    loop n = do let s = n * 1000000
                let t = s + 1000000
                let bs = [check m | m <- [s..t]] `using` parListChunk 1000 rseq
                if or bs then print $ "Found! Base is " ++ show s ++ ", offset is " ++ show (findIndex (== True) bs)
                         else do { putStrLn $ "No match for n = " ++ show n ; loop (n+1) }
