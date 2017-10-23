module Ex3
where
import Data.Char

-- Ex1
subst1 :: String -> String -> String -> String
subst1 oldSub newSub st 
    |substring oldSub st == True = newSub ++ (subst oldSub st)
    |otherwise = subst oldSub st

subst :: String -> String -> String
subst [] st = st
subst _ [] = []
subst oldSub st 
    | prefix oldSub st == True = subst oldSub (drop (length (oldSub))st)
    | prefix oldSub st == False = head(st):(subst oldSub (tail(st)))
 
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
 
-- Ex2
isPalin :: String -> Bool
isPalin [] = True
isPalin st
   |strIgn == reverse(strIgn) = True
   |otherwise = False
   where
    strIgn = ignoreCapitals(st)

ignoreCapitals :: String -> String
ignoreCapitals st = [toLower(ch) | ch <- st, ignorePunctuationSpaces ch spaces]

ignorePunctuationSpaces :: Char -> String -> Bool
ignorePunctuationSpaces ch [] = True
ignorePunctuationSpaces ch (x:xs)
    | ch /= x = ignorePunctuationSpaces ch xs 
    | otherwise = False

whitespaces = ['\n', '\t', ' ']
punctuation = ['.', ',', ';', '-', ':' ]
spaces = whitespaces ++ punctuation

-- Ex3
-- \ - turi skaiciuoti??
-- gale paskaiciuoja kaip zodi
count :: String -> (Int, Int, Int)
count [] = (0,0,0)
count st = (chars, words, lines)
  where 
    chars = length st
    words = length(splitWords st)
    lines = getLinesNum st 1

getWord :: String -> String
getWord [] = []
getWord (x:xs)
    |elem x spaces = []
    | otherwise = x : getWord xs

dropWord :: String -> String
dropWord [] = []
dropWord (x:xs)
    | elem x spaces = (x:xs)    
    | otherwise = dropWord xs

dropSpaces :: String -> String
dropSpaces [] = []
dropSpaces (x:xs)
    |elem x spaces = dropSpaces xs
    |otherwise = (x:xs) 

splitWords :: String -> [String]
splitWords [] = []
splitWords st =
    (getWord new_st) : splitWords(dropWord new_st)
    where
       new_st = dropSpaces st

getLinesNum :: String -> Int -> Int
getLinesNum [] num = num
getLinesNum (x:xs) num
    | x /= '\n' = getLinesNum xs num
    | otherwise = getLinesNum xs (num + 1) --cia pakeisti

-- Ex4 (figovai labas labas labas 5)
justify :: String -> Int -> String
justify st n
    | length(st) < n || n <= 0 = error "Incorect second parameter."
    | otherwise = newLine newSt oldSt n
    where
        oldSt = take n st
        newSt = drop n st

newLine :: String -> String -> Int -> String
newLine [] oldSt n = oldSt
newLine (st:newSt) oldSt n
    | (elem st spaces) && (length(newSt) - 1  <= n) = oldSt ++ "\n" ++ newSt
    | elem st spaces = newLine (drop n newSt) (oldSt ++ "\n" ++ newSt) n
    | otherwise = error "Word exceeds the given line length"

-- Ex5
data Shape = Circle Float (Int,Int)| Rectangle Float Float (Int,Int) deriving (Show, Ord, Eq)
overlaps :: Shape -> Shape -> Bool


{--
overlaps (Rectangle w h (x,y)) (Rectangle w1 h1 (x1,y1))
    |
	where
	 l1x = x + h
	 l2x = x1 + h1
	 r1x = x + w 
	 r2x = x1 + w
	 l1y = y + h
	 l2y = 
--} 
{-- kazkas negerai perdarit is tutorialo
overlaps (Rectangle w h (x,y)) (Rectangle w1 h1 (x1,y1)) = checkOverLap (coord(Rectangle w1 h1 (x,y))) (coord(Rectangle w h (x1,y1)))
checkOverLap :: (Float,Float,Float,Float) -> (Float,Float,Float,Float) -> Bool
checkOverLap (x1,x2,x3,x4) (y1,y2,y3,y4)
    | (x1 <= y1 && x2 >= y1) || (x1 <= y2 && x2 >= y2) || (x3 <= y3 && x3 >= y3)|| (x3 <= y4 && x3 >= y4) = True
    | otherwise = False

coord :: Shape -> (Float,Float,Float,Float)
coord (Rectangle w h (x,y)) = ((fromIntegral x), (fromIntegral x)+w, (fromIntegral y), (fromIntegral y)+h)
--}
overlaps (Circle r1 (x1,y1)) (Circle r2 (x2,y2))
    | distX + distY < ((r1 + r2) * (r1 + r2)) = True
    |otherwise = False
    where
     distX = fromIntegral((x1-x2) * (x1-x2))
     distY = fromIntegral((y1-y2)*(y1-y2))

overlaps (Rectangle w h (x2,y2)) (Circle r (x1,y1)) = overlaps (Circle r (x1,y1)) (Rectangle w h (x2,y2))
overlaps (Circle r (x1,y1)) (Rectangle w h (x2,y2))
    |distX > (w/2 + r) = False
    |distY > (h/2 + r) = False
    |distX <= w/2 = True
    |distY <= h/2 = True
    |dx * dx + dy * dy <= r * r = False
    where
     distX = abs((fromIntegral x1) - (fromIntegral x2) - w/2)
     distY = abs((fromIntegral y1) - (fromIntegral y2) - h/2)
     dx = distX - w/2
     dy = distY - h/2

-- Ex6
-- loan (Person "Eimantas") (Book "knyga" 45) ([(Book1 "knyga" 45 Free),(Book1 "knyga1" 81 Free), (Book1 "knyga2" 99 Loaned)],[Loan (Person "Tomas") (Book "knyga2" 99)])
type Name = String
type Id = Int 
data Person = Person Name deriving (Show, Eq)
data Book = Book Name Id | Book1 Name Id Status deriving (Show, Eq)
data Status = Loaned | Free | Locked deriving (Show, Eq)
data Loan = Loan Person Book deriving (Show, Eq)


loan :: Person -> Book -> ([Book],[Loan]) -> ([Book],[Loan])
loan person (Book bookName id) (xs, ys)
    |goThroughBooks id xs  == True = (changeStatus id xs, addPerson person bookName id ys)
    |otherwise = (xs, ys)

addPerson :: Person -> String -> Int -> [Loan] -> [Loan]
addPerson person bookName id ys = (Loan person (Book bookName id)):ys


changeStatus :: Int -> [Book] -> [Book]
changeStatus _ [] = []
changeStatus id (x:xs)
    |findBook id x == False = [x] ++ (changeStatus id xs)
    |otherwise = [(changeSt x)] ++ (changeStatus id xs)

changeSt :: Book -> Book
changeSt (Book1 name id1 st) = (Book1 name id1 Loaned)

goThroughBooks :: Int -> [Book] -> Bool
goThroughBooks _ [] = False
goThroughBooks id (y:ys)
    |findBook id y  == False = goThroughBooks id ys
    |otherwise = True

findBook :: Int -> Book -> Bool
findBook id (Book1 name id1 st)
    |((id == id1) && (st == Free)) = True
    |otherwise = False