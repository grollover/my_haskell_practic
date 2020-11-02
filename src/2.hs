-- https://www.hackerrank.com/challenges/eval-ex/problem?h_r=next-challenge&h_v=zen
module Lib
    ( someFunc
    ) where

fact :: Int -> Int
fact 0 = 1
fact 1 = 1
fact x = x * fact (x-1)


ex :: Double -> Int -> Double
ex 0 _ = 1
ex _ 0 = 1
ex x n = (x ^^ n) / fromIntegral (fact n) + ex x (n-1)

someFunc :: IO ()
someFunc = print $ ex 20 9