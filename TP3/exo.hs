import Data.Char (toLower)

rangImpairs :: [Int] -> [Int]
rangImpairs [] = []
rangImpairs [x] = []
rangImpairs (x:y:xs) = y:rangImpairs xs

rangPairs :: [Int] -> [Int]
rangPairs [] = []
rangPairs [x] = [x]
rangPairs (x:y:xs) = x:rangPairs xs

fusionGenerique :: (a->a->Bool)->[a] -> [a] -> [a]
fusionGenerique fn l1 [] = l1
fusionGenerique fn [] l1 = l1
fusionGenerique fn (x:xs) (y:ys)
  |fn x y = x:fusionGenerique fn xs (y:ys)
  |otherwise = y:fusionGenerique fn (x:xs) ys


conc :: [Int] -> [Int] -> [Int]
conc [] l1 = l1
conc l1 [] = l1

palindrome :: [Char] -> Bool
palindrome [x] = True
palindrome [] = True
palindrome l
 | head l == ' ' = palindrome (tail l)
 | last l == ' ' = palindrome (init l)
 | toLower (head l) == toLower (last l) = palindrome (tail (init l))
 | otherwise = False

retire :: [Int] -> Int -> [Int]
retire list x = filter (\y -> y /= x) list

permut :: [Int] -> [[Int]]
permut [x , y] = [[y,x],[y,x]]
permut list = [x:p | x<-list , p<-permut( retire list x)]

chain :: Int -> [Int]
chain x
  | x == 1 = [1]
  | even x = x:chain (x `div` 2)
  | otherwise = x:chain(x * 3 + 1)

nbLongChaine :: Int -> Int
nbLongChaine n = length (filter (\y -> length y > n)   (map(\x -> chain x) [1..100]))

-- dec2Bin :: Int -> [Char]
