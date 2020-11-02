f :: [Int] -> [Int]
f [] = []
f (x:xs) = if odd x then x : f xs else f xs

f2 :: [Int] -> Int
f2 arr = sum $ f arr

someFunc :: IO ()
someFunc = print $ f2 [3,2,4,6,5,7,8,0,1]