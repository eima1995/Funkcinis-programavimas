module Ex1
where
import Test.QuickCheck

-- Ex 1.
nAnd :: Bool -> Bool -> Bool
nAnd x y = not(x && y)

nAnd_1 :: Bool -> Bool -> Bool
nAnd_1 True x = not x
nAnd_1 False _ = True


nAnd_2 True True = False
nAnd_2 _ _ = True


-- Ex 2.
prop_nAnd :: Bool -> Bool -> Bool

prop_nAnd x y = (nAnd x y == nAnd_1 x y) && (nAnd x y == nAnd_2 x y)

prop_nAnd2 x y
    | (x == False) || (y == False) = True == (nAnd x y)
    | otherwise = False == (nAnd x y)


-- Ex 3.
nDigits :: Integer -> Int

nDigits n = length(show(abs n))


-- Ex 4

nRoots :: Float -> Float -> Float -> Int
-- 1 5 4; 1 6 9
nRoots a b c
    | a == 0 = error "the first argument should be non-zero!"
    | b^2 > 4.0 * a * c = 2
    | b^2 == 4.0 * a * c = 1
    | otherwise = 0

-- Ex 5

smallerRoot :: Float -> Float -> Float -> Float
largerRoot  :: Float -> Float -> Float -> Float

smallerRoot a b c 
    | nRoots a b c == 0 =  error "no roots"
    | otherwise = ((-b) - sqrt(b^2 - 4 * a * c))/2 *a

largerRoot a b c 
    | nRoots a b c == 0 =  error "no roots"
    | otherwise = ((-b) + sqrt(b^2 - 4 * a * c))/2 *a

-- Ex 6

power2 :: Integer -> Integer

power2 n
    | n == 0 = 1
    | n < 0 = 0
    | otherwise = power2(n-1)*2

-- Ex 7

mult :: Integer -> Integer -> Integer
mult m n
    | m < 0 && n < 0 = mult (-m) (-n)
    | m < 0 && m < n = mult n m
    | n == 0 || m == 0 = 0
   -- | m == 1 = n
    | otherwise = (mult (m - 1) n) + n

prop_mult :: Integer -> Integer -> Bool
prop_mult m n = (mult m n) == (m * n)
    
-- Ex 8

prod :: Integer -> Integer -> Integer

prod m n
  | m > n = error "invalid range"
  | m == n = n
  | otherwise = m * prod (m + 1) n

fact :: Integer -> Integer
fact n = prod 1 n
