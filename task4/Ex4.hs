module Ex4
where
-- Ex1
listLen = sum . map(\x -> 1)

listLen2 xs = foldr (\x -> (+1)) 0 xs

-- Ex2
any1 :: (a -> Bool) -> [a] -> Bool
any1 p x = length(filter p x) > 0

all1 :: (a -> Bool) -> [a] -> Bool
all1 p x = length(filter p x) == length x

any2 p x = foldr (||) False (map p x)
all2 p x = foldr (&&) True (map p x)

-- Ex3
unzip1 x = foldr (\(y, z) (ys, zs) -> (y:ys, z:zs)) ([], []) x

-- Ex4
ff :: Integer -> [Integer] -> Integer
ff num = foldr f 0 . map (*10) . filter (>=0)  
    where 
        f suma y
            | suma + y <= num = y + suma
            | otherwise = y

-- Ex5
flip1 f = \x y -> f y x  

-- Ex6
total1 f n = (sum . map f) [0..n]

-- Ex7
iter1 :: Integer -> (a->a) -> (a->a)
iter1 n f
  | n <= 0 = id
  | otherwise = f . (iter1 (n-1) f)

iter2 :: Integer -> (a->a) -> (a->a)
iter2 n f = foldr (.) id (replicate (fromIntegral n) f)

-- Ex8
splits1 x = map f [0..length(x)]
    where
     f n = ((take n x), (drop n x))
