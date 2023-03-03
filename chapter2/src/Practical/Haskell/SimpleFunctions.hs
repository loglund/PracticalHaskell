module Practical.Haskell.SimpleFunctions where

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty [] = "empty"
firstOrEmpty (x:xs) = x

(+++) :: [a] -> [a] -> [a]
list1 +++ list2 = case list1 of
                    []   -> list2
                    x:xs -> x:(xs +++ list2)

sorted :: [Int] -> Bool
sorted [] = True
sorted [_] = True
sorted (x : r@(y:_)) = x < y && sorted r


myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse (xs) +++ [x]

myMaxMin :: Ord b => [b] -> (b, b)
myMaxMin [x] = (x, x)
myMaxMin (x:xs) = (
                    if x > xs_max then x else xs_max,
                    if x < xs_min then x else xs_min
                    ) where (xs_max, xs_min) = myMaxMin xs

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

ifibonacci :: Int -> Maybe Int
ifibonacci n | n < 0 = Nothing
ifibonacci 0 = Just 0
ifibonacci 1 = Just 1
ifibonacci n | otherwise = let Just f1 = ifibonacci (n-1)
                               Just f2 = ifibonacci (n-2)
                           in Just (f1 + f2)

binom _ 0 = 1
binom x y | x == y = 1
binom n k = (binom (n-1) (k-1)) + (binom (n-1) k)

ackermann :: Int -> Int -> Maybe Int
ackermann 0 0 = Just 1
ackermann n 0 = Just (n + 1)
ackermann 0 m = ackermann 1 (m-1)
ackermann n m = let Just f1 = (ackermann (n-1) m)
                in ackermann (f1) (m-1)