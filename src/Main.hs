module Main where

import System.IO
import System.Environment          (getArgs)

import Data.List                   (findIndex, foldl', nub)
import Data.Digits                 (digits, unDigits)

import Control.Monad               (unless)
import Control.Concurrent          (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.Async    (race)
import Control.Parallel.Strategies (parList, parListChunk, using, rseq, rdeepseq)

import Filters

createFilter :: Int -> Filter -> Filter
createFilter n f = (filter pred zs) --`using` parList rseq
  where
    zs :: [Integer]
    zs = (takeWhile (< lcma) (usefulNumbers f 0)) ++ [lcma]

    lcma :: Integer
    lcma = manyLCM $ nub [k^x | k <- [3,4,5,6], x <- [1..n]]

    pred :: Integer -> Bool
    pred z = let dz = digits 10 z
              in and $ map ((all (\d -> d <= 1)) . (take n) . reverse) [convertBase 10 i dz | i <- [3,4,5,6]]

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
          count <- readFile "count.txt"
          args  <- readFile "prev_result.txt"
          case lines args of
               [arg] -> do filvar <- newMVar maxFilter
                           forkIO $ calc (read arg) filvar
                           loop (read count) filvar
               _     -> error "Wrong result format!"
  where
    loop :: Int -> MVar Filter -> IO ()
    loop m mvarf = do cur <- readMVar mvarf
                      let nf = (createFilter m cur) `using` rdeepseq
                      let o = show (m+1)
                      let newModule = "module StaticFilters.Filter" ++ o ++ " where\n\nfilter" ++ o ++ " :: [Integer]\nfilter" ++ o ++ " = " ++ show nf
                      let filep = "./src/StaticFilters/Filter" ++ o ++ ".hs"
                      writeFile filep newModule
                      writeFile "./count.txt" o
                      putStrLn $ "## New filter! Written to " ++ filep
                      swapMVar mvarf nf
                      loop (m+1) mvarf

    calc :: Integer -> MVar Filter -> IO ()
    calc n f = do fil <- readMVar f
                  let s = take 1000000 (usefulNumbers fil n)
                  let bs = [check m | m <- s] `using` parListChunk 10000 rseq
                  if or bs then do let final = head $ filter check s
                                   writeFile "final_result.txt" (show final)
                                   putStrLn "Found result! Written to final_result.txt"
                           else do let ls = last s
                                   writeFile "./prev_result.txt" (show ls)
                                   putStrLn $ "No match in this block! n ~ 10e" ++ ((show . length . show) ls) ++ " (written)"
                                   calc (n+ls) f
