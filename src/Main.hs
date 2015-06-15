module Main where

import System.IO
import System.Environment          (getArgs)

import Data.List                   (findIndex, foldl', nub)
import Data.Digits                 (digits, unDigits)

import Control.Monad               (unless)
import Control.Concurrent          (forkIO)
import Control.Concurrent.Async    (race_)
import Control.Parallel.Strategies (parList, parListChunk, using, rseq)

import Filters

createFilter :: Int -> Filter
createFilter n = (filter (\z -> (pr 3 z) && (pr 4 z) && (pr 5 z) && (pr 6 z)) zs) `using` parList rseq
  where
    zs   = [0..lcma]
    lcma = manyLCM $ nub [k^x | k <- [3,4,5,6], x <- [1..n]]

    pr :: Int -> Integer -> Bool
    pr i = (all (\d -> d <= 1)) . (take n) . reverse . (convertBase 10 (fromIntegral i)) . (digits 10)

    manyLCM :: (Foldable t, Integral a) => t a -> a
    manyLCM = foldl' lcm (fromInteger 1)

usefulNumbers :: Filter -> Integer -> [Integer]
usefulNumbers fs n = (foldr ((:) . (+n)) (usefulNumbers fs (n + (last fs))) (init fs))

convertBase :: Integral a => a -> a -> [a] -> [a]
convertBase from to = digits to . unDigits from

valid :: Integral a => a -> a -> [a] -> Bool
valid from to = (all (\d -> d <= 1)) . (convertBase from to)

-- Base 2 always valid.
check :: Integer -> Bool
check n = let m = digits 10 n
           in (valid 10 6 m) && (valid 10 5 m) && (valid 10 4 m) && (valid 10 3 m)

-- Assumes argument is a multiple of 60 (for non-silly-number-generation)
main :: IO ()
main = do hSetBuffering stdin NoBuffering
          [arg] <- getArgs
          race_ loop (calc (read arg))
  where
    loop :: IO ()
    loop = do c <- getChar
              if (c == 'q') then putStrLn " Exiting."
                            else putStrLn "Exit at any time by pressing \"Q\"" >> loop

    calc :: Integer -> IO ()
    calc n = do let s = take 1000000 (usefulNumbers filter4 n)
                let bs = [check m | m <- s] `using` parListChunk 10000 rseq
                if or bs then print $ "Found!"
                         else do let ls = last s
                                 putStrLn $ "No match for n <= " ++ show ls
                                 calc ls
