somme :: Int -> Int ->Int
somme x 0 = x
somme x y
    | y > 0 = somme (succ x) (pred y)
    | otherwise  = somme (pred x) (succ y)

moins :: Int -> Int -> Int
moins x 0 = x
moins x y
    | y > 0 = moins (pred x) (pred y)
    | otherwise = moins (pred x) (succ y)

abs' :: Int -> Int
abs' 0 = 0
abs' x
    | x > 0 = x
    | x < 0 = mult (-1) x

mult :: Int -> Int -> Int
mult x 1 = x
mult x y
    | y > 0 =  somme x ( mult x (pred y) )
    | y < 0 = - mult x (abs y)
    | otherwise = 0


quotient :: Int -> Int -> Int
quotient x 0 = error("division par 0 interdite")
quotient x y
    | y > x = 0 
    | y > 0 = succ( quotient (moins x y) y)
    | y < 0 = - quotient x (abs' y)

reste :: Int -> Int -> Int
reste 0 y = 0
reste x y
    | y == 0 = error("division par 0")
    | y > 0 = moins x (mult y (quotient x y))
    | y < 0 = - reste x (abs y)

estPair :: Integer -> Bool
estPair 0 = True
estPair x = not (estPair (pred x))

estImpair :: Integer -> Bool
estImpair x = not (estPair x)

facto :: Int -> Int
facto n
    | n == 0 = 1
    | n > 0 = facto (n - 1) * n
    | otherwise = error ("doit etre >= 0")
    
cnk :: Int -> Int -> Int
cnk n k 
    | n > k = quotient (facto n) (mult (facto k) (facto(moins n k)) )
    | n < k = error("n must be > k")


cnkOpti :: Int -> Int -> Int
cnkOpti n k 
    | n >= k = quotient (product [n-k+1..n]) (facto k)
    | n < k  = error("n must be > k")

-- trinangle :: Int -> String
ligneTriangle' :: Int -> Int -> String
ligneTriangle' x 0 = "1"
ligneTriangle' x y = ligneTriangle' x (pred y) ++ "," ++ show (cnkOpti x y)

ligneTriangle :: Int -> String
ligneTriangle 0 = "[1]\n"
ligneTriangle x = "[" ++ ligneTriangle' x x ++ "]\n"

triangle :: Int -> String
triangle 0 = ligneTriangle 0
triangle x = triangle(pred x) ++ ligneTriangle( x )

printTriangle :: Int -> IO()
printTriangle n = putStr $ triangle (pred n)