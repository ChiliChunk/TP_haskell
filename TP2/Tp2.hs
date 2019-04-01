  
estPremier :: Int -> Bool
estPremier x
  | x < 2 = error("CANNOT BE < 2")
  | otherwise = loop x 2 == x
      where loop w diviseur
              | w `mod` diviseur == 0 = diviseur
              | otherwise = loop w (succ diviseur)

tousPremiers :: Int -> [Int]
tousPremiers x = tousPremiers' x []
  where tousPremiers' w list
          | w < 2 = (1:list)
          | estPremier w == True = tousPremiers' (pred w) (w:list)
          | otherwise = tousPremiers' (pred w) list

estParfait :: Int -> Bool
estParfait x = (sommeDiviseursPropres x) == x


sommeDiviseursPropres ::Int -> Int
sommeDiviseursPropres x = sommeDiviseursPropres' x 1 0
  where sommeDiviseursPropres' x diviseur somme
          | diviseur == x = somme
          | x `mod` diviseur == 0 = sommeDiviseursPropres' x (succ diviseur) (somme + diviseur)
          | otherwise = sommeDiviseursPropres' x (succ diviseur) somme

tousParfaits :: Int -> [Int]
tousParfaits x = tousParfaits' x []
  where tousParfaits' x list
          | x < 2 = (1:list)
          | estParfait x == True = tousParfaits' (pred x) (x:list)
          | otherwise = tousParfaits' (pred x) list
          
amis :: Int -> [(Int , Int)]
amis x = [ (i,j) | i <- [1..x] , j <- [1..x], sommeDiviseursPropres i == j , sommeDiviseursPropres j == i , i /= j]


noDoublons :: [Int] -> [Int]
noDoublons [] = []
noDoublons [x] = [x]
noDoublons (x:y:xs)
  | x==y = noDoublons (y:xs)
  | otherwise = x: noDoublons (y:xs)
          

appartient ::Eq x => x -> [x] -> Bool
appartient _ [] = False
appartient w (x:xs)
  | w == x = True
  | otherwise = appartient w xs

estVoyelle :: Char -> Bool
estVoyelle x = appartient x (['a' , 'e' , 'i' , 'o' , 'u'])

estConsonne :: Char -> Bool
estConsonne x = not (estVoyelle x)


super :: [Int] -> Int
super [x] = x
super (x:xs) = super' x xs
  where super' y (x:xs)
          | xs == [] = if x > y then x else y
          | y > x = super' y xs
          | x > y = super' x xs

conc :: [Int] -> [Int] -> [Int]
conc [] l1 = l1
conc l2 [] = l2
conc (x:xs) l2 = x:conc xs l2

tresse :: [a] -> [b] -> [c] -> [(a,b,c)]
tresse [x] [y] [z] = [(x,y,z)]
tresse (x:xs) (y:ys) (z:zs) = (x,y,z):tresse xs ys zs

-- detresse :: [(a, b, c)] -> ([a], [b], [c])
-- detresse l = ([x | (x, , _) <- l], [y | (, y, ) <- l], [z | (, _, z) <- l])

inserer :: [Int] -> Int -> [Int]
inserer [] w = []
inserer (x:xs) w
  | w < x = w:x:xs   
  | otherwise = x:inserer xs w

compter :: (Int->Bool) -> [Int] ->Int
compter fn [] = 0
compter fn (x:xs)
  | fn x == True = succ(compter fn xs)
  | otherwise = compter fn xs

filtrer ::(Int -> Bool) -> [Int] -> [Int]
filtrer fn [] = []
filtrer fn (x:xs)
  | fn x == True = x:filtrer fn xs
  | otherwise = filtrer fn xs

find ::(Int -> Bool) -> [Int] -> Int
find fn [] = -1
find fn (x:xs)
  | fn x == True = x
  | otherwise = find fn xs