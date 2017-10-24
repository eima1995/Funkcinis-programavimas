module Ex3
where
import Data.Char

-- Ex1
subst :: String -> String -> String -> String
subst[] _ st = st
subst _ _ [] = []
subst oldSub newSub st
    |length(oldSub) > length(st) = st
    |otherwise = subst1 oldSub newSub st ""

subst1 :: String -> String -> String -> String -> String
subst1 oldSub newSub st ats
    |length(oldSub) > length(st) = ats ++ st
    |oldSub /= temp = subst1 oldSub newSub (drop 1 st) (ats ++ (take 1 st))
    |otherwise = subst1 oldSub newSub (drop (length oldSub) st) (ats ++ newSub)
    where
     temp = take (length oldSub) st
    
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

-- Ex3  //gale kad nepaskaiciuotu kaip zodzio
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
    | otherwise = getLinesNum xs (num + 1)

-- Ex4
justify :: String -> Int -> String
justify st n
    | length st < n || n <= 0 = error "Incorect second parameter."
    | otherwise = newLine newSt oldSt n
    where
        oldSt = take n st -- pradzia zodzio
        newSt = drop n st -- pabaiga zodzio

newLine :: String -> String -> Int -> String
newLine [] oldSt n = oldSt
newLine (st:newSt) oldSt n 
    | elem st whitespaces = newLine (drop n newSt) (oldSt ++ "\n" ++ (take n newSt)) n
    | otherwise = error "Word exceeds the given line length"


-- Ex5
-- overlaps (Rectangle 2 2 (1,1)) (Rectangle 2 2 (1,1)) -> True
-- overlaps (Rectangle 2 2 (1,1)) (Rectangle 2 2 (4,1)) -> False
-- overlaps (Rectangle 2 2 (1,1)) (Rectangle 2 2 (3,3)) -> True
-- overlaps (Rectangle 2 2 (1,1)) (Rectangle 2 2 (2,2)) -> True
-- overlaps (Rectangle 2 2 (1,1)) (Rectangle 2 2 (3,0)) -> True
-- overlaps (Rectangle 2 2 (2,2)) (Circle 2 (2,2)) -> True
-- overlaps (Rectangle 2 2 (0,0)) (Circle 1 (4,0)) -> False
-- overlaps (Circle 2 (2,2)) (Circle 2 (1,2)) --> True
-- overlaps (Circle 2 (2,2)) (Rectangle 2 2 (1,1)) --> True
-- overlaps (Circle 1 (5, 0)) (Circle 1 (3, 0)) --> False

-- overlaps (Circle 1 (8,2)) (Rectangle 2 2 (1,1))
-- overlaps (Circle 1 (8,2)) (Rectangle 1 1 (6,2)) --Turetu but false
-- overlaps (Circle 1 (8,2)) (Rectangle 1 1 (6,0)) --Turetu but false
data Shape = Circle Float (Int,Int)| Rectangle Float Float (Int,Int) deriving (Show, Ord, Eq)
overlaps :: Shape -> Shape -> Bool
overlaps (Rectangle w h (x,y)) (Rectangle w1 h1 (x1,y1))
    |l1x > r2x || l2x > r1x = False -- if one rectangle is on left side of other
    |l1y < r2y || l2y < r1y = False -- if one rectangle is above other
    |otherwise = True
    where
     l1x = fromIntegral x     -- l1 - top left coord
     l1y = (fromIntegral y) + h 
     r1x = (fromIntegral x) + w -- r1 - bootom right coord
     r1y = fromIntegral y
     l2x = fromIntegral x1
     l2y = (fromIntegral y1) + h1
     r2x = (fromIntegral x1) + w1
     r2y = fromIntegral y1

overlaps (Circle r1 (x1,y1)) (Circle r2 (x2,y2))
    |distX + distY < ((r1 + r2) * (r1 + r2)) = True
    |otherwise = False
    where
     distX = fromIntegral((x1-x2) * (x1-x2))
     distY = fromIntegral((y1-y2)*(y1-y2))

overlaps (Rectangle w h (x2,y2)) (Circle r (x1,y1)) = overlaps (Circle r (x1,y1)) (Rectangle w h (x2,y2))
overlaps (Circle r (x1,y1)) (Rectangle w h (x2,y2))
    |distX > (w/2 + r) = False -- if the distance is greater than halfCircle + halfRect, then they are too far apart to be colliding
    |distY > (h/2 + r) = False
    |distX <= w/2 = True  -- if the distance is less than halfRect then they are definitely colliding
    |distY <= h/2 = True
    |dx * dx + dy * dy <= r * r = False
    where
     distX = abs((fromIntegral x1) - (fromIntegral x2) - w/2) -- distances between the circle’s center and the rectangle’s center
     distY = abs((fromIntegral y1) - (fromIntegral y2) - h/2)
     dx = distX - w/2
     dy = distY - h/2

-- Ex6
-- loan (Person "Eimantas")(Book "knyga") ([(Book1 "knyga1" 81 Free),(Book1 "knyga" 45 Free),(Book1 "knyga2" 99 Loaned)],[Loan (Person "Tomas") (Book2 "knyga2" 99)])
-- loan (Person "Eimantas")(Book "knyga100") ([(Book1 "knyga1" 81 Free),(Book1 "knyga" 45 Free),(Book1 "knyga2" 99 Loaned)],[Loan (Person "Tomas") (Book2 "knyga2" 99)])
-- loan (Person "Eimantas")(Book "knyga1") ([(Book1 "knyga1" 81 Locked),(Book1 "knyga" 45 Free),(Book1 "knyga2" 99 Loaned)],[Loan (Person "Tomas") (Book2 "knyga2" 99)])
type Name = String
type Id = Int 
data Person = Person Name deriving (Show, Eq)
data Book = Book Name | Book1 Name Id Status | Book2 Name Id deriving (Show, Eq)
data Status = Loaned | Free | Locked deriving (Show, Eq)
data Loan = Loan Person Book deriving (Show, Eq)

loan :: Person -> Book -> ([Book],[Loan]) -> ([Book],[Loan])
loan person (Book bookName) (xs, ys) = goThroughBooks person bookName xs ys []

goThroughBooks :: Person -> String -> [Book] -> [Loan] -> [Book] -> ([Book],[Loan])
-- goThroughBooks _ _ [] _ _ = (temp, xs)
goThroughBooks person bookName (y:ys) xs temp
    |findBook bookName y  == True = ((reverse(temp) ++ (changeSt y):ys), addPerson person bookName (getBookID y) xs)
    |ys == [] = (reverse((y:temp)), xs) 
    |otherwise = goThroughBooks person bookName ys xs (y:temp)

findBook :: String -> Book -> Bool
findBook bookName (Book1 name id1 st)
    |((bookName == name) && (st == Free)) = True
    |otherwise = False

changeSt :: Book -> Book
changeSt (Book1 name id st) = (Book1 name id Loaned)

getBookID :: Book -> Int
getBookID (Book1 name id st) = id

addPerson :: Person -> String -> Int -> [Loan] -> [Loan]
addPerson person bookName id ys = (Loan person (Book2 bookName id)):ys
