module Main where

import System.IO
import System.Environment          (getArgs)

import Data.List                   (findIndex)
import Data.Digits                 (digits, unDigits)

import Control.Monad               (unless)
import Control.Concurrent          (forkIO)
import Control.Concurrent.Async    (race_)
import Control.Parallel.Strategies (parListChunk, using, rseq)

type Filter = [Integer]

basicFilter :: Filter
basicFilter = [0,1,25,36,60]

usefulNumbers :: Integer -> Filter -> [Integer]
usefulNumbers n (fs:f:[]) = (foldr ((:) . (n+)) [] fs) : (usefulNumbers (n + f) fs)

nonSillyNumbers :: Integer -> [Integer]
nonSillyNumbers n = n:(n+1):(n+25):(n+36):(nonSillyNumbers (n+60))

convertBase :: Integral a => a -> a -> [a] -> [a]
convertBase from to = digits to . unDigits from

valid :: Integral a => a -> a -> [a] -> Bool
valid from to = (all (\d -> (d <= 1))) . (convertBase from to)

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
calc n = do let s = take 1000000 (nonSillyNumbers n)
            let bs = [check m | m <- s] `using` parListChunk 10000 rseq
            if or bs then print $ "Found!"
                     else do let ls = last s
                             putStrLn $ "No match for n <= " ++ show ls
                             calc ls
