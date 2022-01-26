{-# LANGUAGE BangPatterns #-}
import Data.Char (toUpper)

myFun x = 2 * x


add2T :: Num a => (a, a) -> a
add2T (x,y) = x + y


add2C :: Num a => a -> a -> a
add2C x y = x + y


add3T :: Num a => (a, a, a) -> a
add3T (a,b,c) = a + b + c


add3C :: Num a => a -> a -> a -> a
add3C a b c = a + b + c

curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f a b = f(a,b)

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f (a,b) = f a b

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f(a,b,c)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c

-- Zadania 2.

fiveToPower_ :: Integer -> Integer
fiveToPower_ x = (^ x) 5

_ToPower5 :: Num a => a -> a
_ToPower5 a = (^ 5) a

subtrNFrom5 :: Num a => a -> a
subtrNFrom5 a = (5 -) a

subtr5From_ :: Num a => a -> a
subtr5From_ a = a - 5 -- (- 5) doesnt work sadge


isPalindrome :: [Char] -> Bool
isPalindrome s = reverse s == s


getElemAtIdx :: [a] -> Int -> a --zakladam ze lista jest intem
getElemAtIdx list idx = head (drop idx list)

capitalize :: [Char] -> [Char]
capitalize w =  toUpper (head w) : drop 1 w


-- Napisać wyrażenie obliczające, ile jest w przedziale [1,100] 
-- trójek liczb całkowitych reprezentujących długości boków trójkąta prostokątnego
trojki :: Int -> Int
trojki n = length [(a,b,c) | a <- [1..n], b<- [a..n], c<-[a..n], a+b>c, a^2+b^2==c^2]

isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []


isPrimeBetter :: Integral t => t -> Bool
isPrimeBetter 1 = False
isPrimeBetter n =  [i | i <- [2..range],n `mod` i == 0] == []
    where range = round (sqrt (fromIntegral n))


primesInRange :: Int -> Int
primesInRange range = length [i | i<-[1..range], isPrimeBetter i]


primes :: [Int]
primes = eratoSieve [2..]
 where
   eratoSieve :: [Int] -> [Int]
   eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

isPrime2 :: Int -> Bool
isPrime2 n = length [primes !! el | el <-[0..n], primes !! el == n] == 1


allEqual :: Eq a => [a] -> Bool
allEqual xs = length xs == length [1 | cnt<-[0..length xs-1], xs!!cnt == head xs]

fib :: (Num a, Eq a) => a -> a
fib n =
 if n == 0 || n == 1 then n
 else fib (n - 2) + fib (n - 1)


sum' :: Num a => [a] -> a
sum' []   = 0
sum' (x:xs) = x + sum' xs



prod' :: Num a => [a] -> a -- prod' [1,2,3] = 6
prod' [] = 1
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int -- length' [1,1,1,1] = 4
length' [] = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool -- or' [True, False, True] = True
or' [] = True
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool -- and' [True, False, True] = False
and' [] = True
and' (x:xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool -- elem' 3 [1,2,3] = True
elem' n [] = False
elem' n (x:xs) = x == n || elem' n xs

doubleAll :: Num t => [t] -> [t] -- doubleAll [1,2] = [2,4]
doubleAll [] = []
doubleAll (x:xs) = x*2 : doubleAll xs

squareAll :: Num t => [t] -> [t] -- squareAll [2,3] = [4,9]
squareAll [] = []
squareAll (x:xs) = x^2 : squareAll xs

selectEven :: Integral t => [t] -> [t] -- selectEven [1,2,3] = [2]
selectEven [] = []
selectEven (x:xs) = if even x then x:selectEven xs else selectEven xs


avg :: [Float] -> Float
avg xs = sum xs/fromIntegral (length xs)

geoavg :: [Float] -> Float
geoavg xs = product(xs)**(1/fromIntegral (length xs))

avgs:: [Float] -> (Float, Float)
avgs xs = (sum xs/fromIntegral (length xs),product(xs)**(1/fromIntegral (length xs)))

sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs


sum'3 :: Num a => [a] -> a
sum'3 = loop 0
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs

prod'2 :: Num a => [a] -> a
prod'2 = loop 1
 where loop acc []     = acc
       loop acc (x:xs) = loop (acc * x) xs

length'2 :: [a] -> Int
length'2 = loop 0
    where loop acc [] = acc
          loop acc (x:xs) = loop (acc + 1) xs

sum'4 :: Num a => [a] -> a
sum'4 = loop 0
   where loop !acc []     = acc
         loop !acc (x:xs) = loop (x + acc) xs

qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart  xs = [ y | y <- xs, y <= x ]
   rightPart xs = [ y | y <- xs, y > x  ]


qSort2 :: Ord a => [a] -> [a]
qSort2 []     = []
qSort2 (x:xs) = qSort2 (leftPart xs) ++ [x] ++ qSort2 (rightPart xs)
 where
   leftPart xs = filter (<=x) xs
   rightPart xs = filter (>x) xs



merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
    | x <= y     = x:merge xs (y:ys)
    | otherwise  = y:merge (x:xs) ys


mSort :: Ord a => [a] -> [a]
mSort [] = []
mSort [x] = [x]
mSort xs = merge (mSort (leftPart xs)) (mSort (rightPart xs))
    where
        leftPart xs = take ((length xs + 1) `div` 2) xs
        rightPart xs = drop ((length xs + 1) `div` 2) xs


insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs)
    | n < x = n : x : xs
    | otherwise = x : insert n xs


iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort [x] = [x]
iSort (x:xs) = loop [x] xs
    where loop acc [] = acc
          loop acc (x:xs) = loop(insert x acc) xs


fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False


fstDivSnd :: Integral a => [a] -> Bool
fstDivSnd (x : y : _) | y `mod` x == 0 = True
fstDivSnd _                            = False