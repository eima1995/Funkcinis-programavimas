module Ex5
where
import qualified Data.Set as S
import Data.List  (lookup)
import Data.Maybe (fromJust)

-- Ex1
-- eval [] (Lit 4)
-- eval [('a',2),('b',3)] (EVar 'b')
-- eval valuation expression
data Expr a = Lit a | EVar Var | Op (Ops a) [Expr a]
type Ops a = [a] -> a
type Var = Char
type Valuation a = [(Var, a)]

eval :: Valuation a -> Expr a -> a
eval _ (Lit x) = x
eval vals (EVar var) = fromJust $ lookup var vals
eval vals (Op f exprs) = f $ map (eval vals) exprs

expression :: Expr Integer
expression = Op sum [Op product [EVar 'a', EVar 'b'], Lit 2] -- a * b + 2

valuation :: Valuation Integer
valuation = [('a', 5), ('b', 3), ('c', 2)]

-- Ex2
-- Nlist[1,2,3] == Nlist[1,2,3]
--Nlist[1,2] == Nlist[1,2,3]
-- Nlist[1,2,3] <= Nlist[1,2,10]
data NumList a = Nlist[a]

average :: Fractional a => [a] -> a
average [] = 0
average a = sum a / fromIntegral(length a)

instance (Fractional a, Eq a) => Eq (NumList a) where
     (Nlist a) == (Nlist b) = average a == average b    

instance (Fractional a, Ord a) => Ord (NumList a) where
  compare (Nlist a) (Nlist b) = compare (average a) (average b)
  
-- Ex3
instance (Num a, Num b) => Num (a->b) where
  a + b = \x -> a x + b x
  a * b = \x -> a x * b x
  negate a = \x -> -(a x)
  abs a = abs . a
  signum a = signum . a
  fromInteger a = fromInteger a

-- Ex4
-- depth(Gnode[Leaf 1, Leaf 2, Gnode[Leaf 3, Gnode[Leaf 4, Gnode [Leaf 5]]]])
-- findElem 2 (Gnode [Leaf 1, Leaf 2, Leaf 3, Gnode[Leaf 4, Leaf 5, Gnode[Leaf 6, Leaf 7]]])
-- mapTree (+1) (Gnode [Leaf 1, Leaf 2, Leaf 3, Gnode[Leaf 4, Leaf 5, Gnode[Leaf 6, Leaf 7]]])
data GTree a = Leaf a | Gnode [GTree a] deriving (Eq, Show)

depth :: GTree a -> Integer
depth (Leaf _) = 0
depth (Gnode xs) = 1 + maximum [depth branch | branch <- xs]

findElem :: (Eq a) => a -> GTree a -> Bool
findElem x (Leaf y) = x == y
findElem x (Gnode xs) = any (findElem x) xs

mapTree :: (a -> b) -> GTree a -> GTree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Gnode xs) = Gnode [mapTree f branch | branch <- xs]

-- Ex5
data Result a = OK a | Error String deriving Show
composeResult :: (a -> Result b) -> (b-> Result c) -> (a -> Result c)
composeResult f g = \x -> case f x of
  OK y -> g y
  Error output -> Error output
{--
errorFunction :: a -> Result a
errorFunction _ = Error "Some error"

okFunction1 :: Integer -> Result Double
okFunction1 = OK . fromIntegral

okFunction2 :: Double -> Result String
okFunction2 = OK . show

main = do
  print $ composeResult errorFunction okFunction1 $ 1
  print $ composeResult okFunction1 errorFunction $ 2
  print $ composeResult okFunction1 okFunction2 $ 3
  print $ composeResult errorFunction errorFunction $ 4 
--}

-- Ex6
-- dom (S.fromList([(1,5), (2,6), (3,7)])) 
-- ran (S.fromList([(1,2), (1,3), (2,4)])) = [2,3,4]
-- ran (S.fromList([(1,'2'), (1,'3'), (1,'4')])) = "234"
-- image (S.fromList([1])) (S.fromList([(1,2), (1,3), (1,4)])) 
type Relation a b = S.Set (a,b)
dom :: Ord a => Relation a b -> S.Set a
dom xs = S.fromList [fst a| a <- S.toList xs]

ran :: Ord b => Relation a b -> S.Set b
ran xs = S.fromList [snd a | a <- S.toList xs]
 
image :: (Ord a, Ord b) => S.Set a -> Relation a b -> S.Set b
image xs ys = S.fromList [b | (a,b) <- snd, elem a fst]
  where
    fst = S.toList xs
    snd= S.toList ys

-- Ex7
primes :: [Integer]
primes = sieve [2..]
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x > 0]

goldbach :: Integer -> Bool
goldbach n = all (\x -> elem x sums) evens
  where
    evens = [x | x <- [4..n], even x]
    primesNum = takeWhile (\x -> x <= n) primes
    sums = [x+y | x <- primesNum, y <- primesNum]

-- Ex8
data Stream a = Cons a (Stream a)

-- f n = (Cons n (f (n+1)))
-- take 100 (streamtoList (f 1))
streamtoList :: Stream a -> [a]
streamtoList (Cons x xs) = x : streamtoList xs

-- take 100 (streamtoList (streamIterate (\x -> x + 1) 5))
streamIterate :: (a -> a) -> a -> Stream a
streamIterate f seed = Cons seed (streamIterate f (f seed))

-- x = Cons 0 x
-- y = Cons 1 y
-- take 10 (streamtoList (streamInterleave x y))
streamInterleave :: Stream a -> Stream a -> Stream a 
streamInterleave (Cons x xs) ys = Cons x (streamInterleave ys xs)