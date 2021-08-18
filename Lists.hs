module Lists where
  import Data.List

  --returns an unbounded list starting at 1
  countingNumbers :: [Int]
  countingNumbers = [1..]

--gets list from countingNumbers and multiplies by 2 to get all even integers
  evenNumbers :: [Int]
  evenNumbers = map (*2) countingNumbers

--gets a list of numbers that divide x, doesn't count 1 as a divisor
  divList :: Int -> [Int]
  divList x = [ y | y <- [2..(x `div` 2)], x `mod` y == 0] ++ [x]

  --uses divList to see if x has only one divisor (we removed the 1 in divList), and returns a True if so. Otherwise, returns a False
  prime :: Int -> Bool
  prime x = if x == 1 then False else divList x == [x]

  --uses prime to determine whether numbers from countingNumbers is prime, and if so, adds it to the list
  primeNumbers :: [Int]
  primeNumbers = [ x | x <- countingNumbers, prime x]

  --compares the first element of each list, chooses the lowest, and then moves on to sort all numbers in both lists into one list
  merge :: [Int] -> [Int] -> [Int]
  merge [] x = x
  merge x [] = x
  merge (x:a) (y:b) | y < x = y : merge (x:a) b
  merge (x:a) (y:b) | otherwise = x : merge a (y:b)
  
  wrap :: Int -> [Int] -> [Int]
  wrap a b = take (length b) (drop a (cycle b))

  --takes the first elements from y and drops the first elements, according to the numbers in x.
  slice :: (Int,Int) -> [Int] -> [Int]
  slice x y = drop (fst x - 1) (take (snd x) y)

  --takes the first n elements and adds them as a list to the list, then does it again with n+1 until we reach the length of x
  subLists :: [Int] -> [[Int]]
  subLists x = [take n x | n <- [1..length x]]

  --flattens the array using concat and returns the length
  countElements :: [[Int]] -> Int
  countElements [] = 0
  countElements [[]] = 0
  countElements a = length (concat a)

  sortSubLists :: (Ord n, Num n) => [[n]] -> [[n]]
  sortSubLists = sortOn sum . map sort
  
  listApply :: Foldable t => (a -> a -> a) -> [t a] -> [a]
  listApply f [] = []
  listApply f (x:xs) = foldr1 f x : listApply f xs
  
  composeList :: [n -> n] -> n -> n
  composeList [] n = n
  composeList (x:xs) b = x (composeList xs b)