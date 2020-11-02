-- https://www.hackerrank.com/challenges/area-under-curves-and-volume-of-revolving-a-curv
module Lib
    ( someFunc
    ) where

delta :: Double
delta = 0.001

ci :: Int -> Int -> Int -> Int -> Double
ci a b n i = fromIntegral a + (fromIntegral (b - a) / fromIntegral n) * fromIntegral i

fx :: Double -> [Int] -> [Int] -> Double
fx x kList powerList = sum [fromIntegral k*(x ^^ power) | (k, power) <- zip kList powerList]

sfx :: Int -> Int -> [Int] -> [Int] -> Double
sfx l r a b = sum [(fx (ci l r (intervalCount l r) i) a b)*delta | i <- [1..(intervalCount l r)]] where
    intervalCount :: Int -> Int -> Int
    intervalCount a b = (b - a) * round (delta ^^ (- 1))

vfx :: Int -> Int -> [Int] -> [Int] -> Double
vfx l r a b = sum [(pi * (fx (ci l r (intervalCount l r) i) a b) ^^ 2)*delta | i <- [1..(intervalCount l r)]] where
    intervalCount :: Int -> Int -> Int
    intervalCount a b = (b - a) * round (delta ^^ (- 1))

solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = [sfx l r a b, vfx l r a b]

someFunc :: IO ()
-- someFunc = print $ sfx 1 4 [1] [3]
someFunc = print $ solve 1 4 [1, 2, 3, 4, 5] [6, 7, 8, 9, 10]
-- someFunc = print $ fx 5 [1, 2, 3, 4, 5] [6, 7, 8, 9, 10]