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

fac :: (Integral a) => a -> a
fac n = product [1..n]

expApproxUpTo :: (Floating a, Integral a) => a -> a -> a
expApproxUpTo n x = sum ([func (a,x) | a<-[0..n]])
            where func x = (snd x ** fst x)/ fac (fst x)