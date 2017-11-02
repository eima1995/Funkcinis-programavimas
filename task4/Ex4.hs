module Ex4
where
-- Ex1
listLen :: [a] -> Int
listLen = sum . map(\x -> 1)

listLen2 :: [a] -> Int
listLen2 = foldr (+) 0 . map(\x -> 1)

-- Ex2
any1 :: (a -> Bool) -> [a] -> Bool
any1 p x
    |length(filter p x) > 0 = True
    |otherwise = False

all1 :: (a -> Bool) -> [a] -> Bool
all1 p x
    |length(filter p x) == length x = True
    |otherwise = False

any2 p x = foldr (||) False (map p x)
all2 p x = foldr (||) False (map p x)

-- Ex3
--unzip1 [(1,2),(2,3),(3,4)]
unzip1 xs = foldr f x xs
  where
   f (x,y)(xs,ys) = (x:xs, y:ys)
   x = ([],[])

-- Ex4
ff maxNum = sum1 . filter(>0) . map(*10)
{--   where
    sum1 x = foldr (+) 0 . map(\x -> 1)
--}
sum1 = foldr (+) 0 . map(\x -> x)
{--
rev ::  [a] -> a
rev xs = foldr snoc x xs
snoc ::  a -> [a] -> a
snoc y x:xs
	| y < 10 = y + x
	|otherwise = y
--}
-- Ex5
flip1 f = \x y -> f y x  

 