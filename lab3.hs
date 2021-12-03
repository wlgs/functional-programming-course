import Data.Char
import Data.List
-- KARTKOWKA
-- zadanie 1:  otrzymujemy list comprehension
--             piszemy jako map/filter/fold
-- zadanie 2:  zadanie z operatorów kropki, dolara,
--             napisać co to wyrażenia zwróci
-- zadanie 3:  zrealizować collection pipeline
--             które da określony output


-- Napisać funkcje anonimowe (wyrażenia lambda) odpowiadające:

-- f1(x)=x−2; x∈ℝ
-- f2(x,y)=(√x2+y2); x,y∈ℝ
-- f3(x,y,z)=(√x2+y2+z2); x,y,z∈ℤ
-- Uwaga: Sprawdzić działanie dla wybranych wartości argumentów


f1 :: Integer -> Integer
f1 = \x -> x - 2


f2 :: (Double, Double) -> Double
f2 = \(x,y) -> (x**2+y**2)**0.5


f3 :: (Double, Double, Double) -> Double
f3 = \(x,y,z) -> (x**2+y**2+z**2)**0.5


-- Napisać funkcje anonimowe (wyrażenia lambda) odpowiadające:
-- (2*), (*2), (2^), (^2), (2/), (/3), (4-)
-- Uwaga: jw.

a1 :: Integer -> Integer
a1 = \x -> 2*x

a2 :: Integer -> Integer
a2 = \x -> x*2

a3 :: Double -> Double
a3 = \x -> x**2

a4 :: Double -> Double
a4 = \x -> 2**x

a5 :: Double -> Double
a5 = \x -> 2/x

a6 :: Double -> Double
a6 = \x -> x/3

a7 :: Integer -> Integer
a7 = \x -> 4-x


-- Npisać funkcje anonimowe (wyrażenia lambda) odpowiadające:

-- f7 x = if x `mod` 2 == 0 then True else False

-- f8 x = let y = sqrt x in 2 * y^3 * (y + 1)

-- f9 1 = 3
-- f9 _ = 0
-- Uwaga: jw.


f7 :: Integer -> Bool
f7 = \x -> x `mod` 2 == 0

f8 :: Double -> Double
f8 = \x -> 2 * (x**0.5)**3 * (x**0.5)+1

f9 :: Integer -> Integer
f9 = \x -> if x==1 then 3 else 0


sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

-- Na podstawie powyższej definicji napisać funkcję sumSqr' (sumującą kwadraty elementów listy)

sumSqr' :: Num a => [a] -> a
sumSqr' []     = 0
sumSqr' (x:xs) = (x^2) + sum' xs

-- Zdefiniować funkcję
-- sumWith :: Num a => (a -> a) -> [a] -> a
-- sumWith f ...

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f [] = 0
sumWith f (x:xs) = f(x) + sumWith f xs

-- Wykorzystując funkcję sumWith zdefiniować
-- sum     = sumWith ...
-- sumSqr  = sumWith ...
-- sumCube = sumWith ...
-- sumAbs  = sumWith ...

sum'2 :: [Integer] -> Integer
sum'2 = sumWith (\x -> x)
sumSqr :: [Double] -> Double
sumSqr = sumWith (\x -> x^2)
sumCube :: [Double] -> Double
sumCube = sumWith (\x -> x^3)
sumAbs :: [Integer] -> Integer
sumAbs = sumWith (\x -> if x>0 then x else -x)

-- Wykorzystując sumWith (bez definiowania nowej funkcji) obliczyć w GHCi
-- ∑15i=1i5

-- sumWith (\x -> x^5) [1..15]

-- Wykorzystując funkcję sumWidth zdefiniować
-- listLength = sumWith ...


listLength :: Num a => [a] -> a
listLength = sumWith (\x -> 1)


sqr :: Num a => a -> a
sqr x = x^2

funcFactory :: (Num a, Eq a) => a -> a -> a
funcFactory n = case n of
 1 -> id
 2 -> sqr
 3 -> (^3)
 4 -> \x -> x^4
 5 -> intFunc
 _ -> const n
 where
   intFunc x = x^5


-- Napisać funkcję
-- expApproxUpTo :: Int -> Double -> Double
-- expApproxUpTo n = ...
-- zwracającą rozwinięcie funkcji ex w szereg MacLaurina o długości n+1, n < 6, tzn. expApproxUpTo n = ∑nk=0xkk!.
-- Czy da się to rozwiązanie uogólnić tak, aby funkcja expApproxUpTo zwracała rozwinięcia dla dowolnego n?

expApproxUpTo :: (Enum a, Floating a) => a -> a -> a
expApproxUpTo n x = sum [ func k x | k<-[0..n]]
              where func k x = x ** k/fac k
                    fac n = product [1..n]


------


funcList :: [ Double -> Double ]
funcList = [ \x -> (sin x)/x, \x -> log x + sqrt x + 1, \x -> (exp 1) ** x ]

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x [] = []
evalFuncListAt x (f:fs) = f x : evalFuncListAt x fs


displEqs :: (Double -> Double, Double -> Double)
displEqs = (\t -> 4 * t^2 + 2 * t, \t -> 3 * t^2)



-- Wykorzystując funkcje sort i reverse oraz operator . napisać funkcję sortującą malejąco podaną listę

sortDesc :: Ord a => [a] -> [a]
sortDesc xs = (reverse . sort ) xs

-- Przepisać funkcję sortDesc do wersji point-free
sortDescPF :: Ord a => [a] -> [a]
sortDescPF xs = reverse (sort xs)



-- Napisać funkcję
are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool
are2FunsEqAt f g [] = True
are2FunsEqAt f g xs = f (head xs)==g (head xs) && are2FunsEqAt f g (drop 1 xs)



onlyEven :: Integral a => [a] -> [a]
onlyEven [] = []
onlyEven (x:xs)
 | x `mod` 2 == 0 = x : onlyEven xs
 | otherwise      = onlyEven xs

-- Napisać (wg podanego schematu) definicje funkcji onlyOdd i onlyUpper

-- onlyOdd [1..10] -- [1,3,5,7,9]
-- onlyUpper "My name is Inigo Montoya. You killed my father. Prepare to die." -- "MIMYP"

onlyOdd :: Integral a => [a] -> [a]
onlyOdd [] = []
onlyOdd (x:xs) = if x`mod`2==1 then x:onlyOdd xs else onlyOdd xs


onlyUpper :: [Char] -> [Char]
onlyUpper "" = ""
onlyUpper (x:xs) = if isUpper x then x : onlyUpper xs else onlyUpper xs


-- Uogólnić poprzednie rozwiązania wprowadzając funkcję filter'

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) = if p x then x: filter' p xs else filter' p xs

onlyEven2 :: [Integer] -> [Integer]
onlyEven2  = filter' (\x -> x`mod`2==1)
onlyOdd2 :: [Integer] -> [Integer]
onlyOdd2   = filter' (\x -> x`mod`2==0)
onlyUpper2 :: [Char] -> [Char]
onlyUpper2 = filter' isUpper

-- Przepisać używając list comprehensions
-- length (filter even [1..10^6])
-- [n | n<-[1..10^6], even n]


doubleElems :: Num a => [a] -> [a]
doubleElems []     = []
doubleElems (x:xs) = 2 * x : doubleElems xs


-- Uogólnić poprzednie rozwiązania wprowadzając funkcję map'

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f xs = (f $ head $ xs) : map' f (drop 1 xs)

doubleElems2 :: [Integer] -> [Integer]
doubleElems2 = map' (*2)
sqrElems :: [Double] -> [Double]
sqrElems    = map' (\x -> sqrt(x))
lowerCase :: [Char] -> [Char]
lowerCase   = map' toLower


------

sumWith2 :: Num p => (p -> p) -> [p] -> p
sumWith2 g []     = 0
sumWith2 g (x:xs) = g x + sumWith g xs -- (+) (g x) (sumWith g xs)

prodWith :: Num p => (t -> p) -> [t] -> p
prodWith g []     = 1
prodWith g (x:xs) = g x * prodWith g xs -- (*) (g x) (prodWith g xs)



sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' = go 0
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x + acc) g xs

prodWith' :: Num a => (a -> a) -> [a] -> a
prodWith' = go 1
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x * acc) g xs



foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x $ foldr' f z xs

sumWith'' :: Num a => (t -> a) -> [t] -> a
sumWith'' g = foldr' (\x acc -> g x + acc) 0
prodWith'' :: Num a => (t -> a) -> [t] -> a
prodWith'' g = foldr' (\x prod -> g x * prod) 1


-- Wykorzystując zip lub zipWith napisać funkcję
-- isSortedAsc [1,2,2,3] -> True, isSortedAsc [1,2,1] -> False

isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = length ( filter (\(a,b)->a<=b) (zip xs (tail xs))) == length xs - 1


-- Napisać funkcję
-- everySecond [1..8] -> [1,3,5,7]
-- ?????
-- everySecond :: [t] -> [t]
-- everySecond xs = ... 


concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ concat' xs


-- Napisać definicję funkcji concat wykorzystując
-- list comprehension

concatList :: [[a]] -> [a]
concatList [] = []
concatList xs = [(xs!!i)!!j | i<-[0..(length xs-1)], j<-[0..(length (xs!!i)-1)]]

-- foldr
concatFoldr :: [[a]] -> [a]
concatFoldr = foldr (++) []


capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize (x:xs) = toUpper x : (map toLower xs)

formatStr :: String -> [Char]
formatStr s = foldr1 (\w s -> w ++ " " ++ s) .
          map capitalize .
          filter (\x -> length x > 1) $
          words s
          

prodPrices :: Num p => String -> p
prodPrices p = case p of
 "A" -> 100
 "B" -> 500
 "C" -> 1000
 _   -> error "Unknown product"

products = ["A","B","C"]

-- basic discount strategy
discStr1 :: (Ord a, Fractional a) => String -> a
discStr1 p
 | price > 999 = 0.3 * price
 | otherwise   = 0.1 * price
 where price = prodPrices p

-- flat discount strategy
discStr2 :: Fractional a => String -> a
discStr2 p = 0.2 * prodPrices p

totalDiscout discStr =
 foldl1 (+) .
 map discStr .
 filter (\p -> prodPrices p > 499)