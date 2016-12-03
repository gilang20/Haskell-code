import Data.List

head' (x:xs) = x

tail' (x:xs) = xs

last' [x] = x
last' (x:xs) = last' xs

sum' [] = 0
sum' (x:xs) = x + sum' xs

product' [] = 1
product' (x:xs) = x * product' xs

length' [] = 0
length' (x:xs) = 1 + length' xs

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

init' [x] = []
init' (x:xs) = x : init' xs

take' n ls
  | n <= 0 = []
  | null ls = []
  | otherwise = head ls : take' (n-1) (tail ls)
  
drop' n ls
  | n <= 0 = ls
  | null ls = ls
  | otherwise = drop' (n-1) (tail ls)

quicksort1 [] = []
quicksort1 (x:xs) = quicksort1 (filter (<= x) xs) ++ [x] ++ quicksort1 (filter (> x) xs)
