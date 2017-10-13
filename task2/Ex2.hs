module Ex2
where 
import Data.Char

-- Ex1
average,suma :: [Float] -> Float
average(x) = suma(x) / fromIntegral(length(x))
suma[] = 0 
suma(x:xs) = x + suma (xs)

-- Ex2
divides :: Integer -> [Integer]
divides x
   | x == 1 = [x]
   | x > 0 = dividers [x] (div x 2)
   |otherwise = []

dividers :: [Integer] -> Integer -> [Integer]
dividers (x) y
   | y == 1 = y:x
   | mod (last(x)) y == 0 = dividers(y:x) (y-1)
   | otherwise = dividers x (y-1)

divides2:: Integer -> [Integer]
divides2 x = [ y | y <- [1..x], mod x y == 0]

isPrime :: Integer -> Bool
isPrime x 
    | x < 0 = error "Function can only check non-negative numbers"
    | otherwise = [1, x] == divides x

-- Ex3
prefix :: String -> String -> Bool
prefix[][] = True
prefix [] _ = True
prefix _ [] = False
prefix (x:xs)(y:ys)
    | x == y = prefix xs ys
    | otherwise = False

substring :: String -> String -> Bool
substring _ [] = False
substring x y
  | prefix x y == True = True
  | otherwise = substring x (tail y)

-- Ex4
remove :: [Integer] -> Integer -> [Integer]
remove [] _ = []
remove (y:ys) x
   | x == y = remove ys x
   | otherwise = y:(remove ys x)

permut :: [Integer] -> [Integer] -> Bool
permut [] [] = True
permut [] y = False
permut x [] = False
permut (x:xs) (y:ys)
    |length(xs) /= length(ys) = False
    |otherwise = permut(remove (x:xs) x) (remove (y:ys) x)

-- Ex5 
is_Letter :: Char -> Bool
is_Letter ch
    |(ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') = True
    |otherwise = False

capitalise :: String -> String
capitalise st = [toUpper(ch) | ch <- st, is_Letter ch]

-- Ex6
add :: (String, Float) -> [(String, Float)] -> [(String, Float)]
add x [] = [x]
add x (y:ys)
  | fst x == fst y = (fst x, snd x + snd y):ys
  | otherwise = y : add x ys

itemTotal :: [(String, Float)] -> [(String, Float)]
itemTotal [] = []
itemTotal [x] = [x]
itemTotal (x:xs) = add x (itemTotal xs)

itemDiscount :: String -> Integer -> [(String, Float)] -> [(String, Float)]
itemDiscount _ _ [] = []
itemDiscount name discount (x:xs)
  | discount < 0 || discount > 100 = error "The second parameter, ranging from 0% to 100%)!"
  | name == fst x = (name, snd x * (100 - fromIntegral(discount))/100) : itemDiscount name discount xs
  | otherwise = x : itemDiscount name discount xs