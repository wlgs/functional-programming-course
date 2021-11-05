-- na next kartkowce sekcja operatorow lub partial aplication
-- oraz zadanie typu lista i co wyjdzie (?)
-- uzupełnić kawałki kodu by lista była uzupełniona
-- zadanie z rekurencji wiedziec ocb znac terminy
-- czyli
-- 4 zadania
-- sekcja operatorow
-- proste funkcje listowe
-- list comprehension
-- rekurencja


myFun x = 2 * x


add2T :: Num a => (a, a) -> a
add2T (x,y) = x + y


add2C :: Num a => a -> a -> a
add2C x y = x + y


add3T :: Num a => (a, a, a) -> a
add3T (a,b,c) = a + b + c


add3C :: Num a => a -> a -> a -> a
add3C a b c = a + b + c

-- here I will do curry functions dx

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


getElemAtIdx :: [Int] -> Int -> [Int] --zakladam ze lista jest intem
getElemAtIdx list idx = head (drop (idx-1) list)           


-- ^ to do ;0000000