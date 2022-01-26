{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Distribution.Simple.PackageIndex (searchByNameSubstring)
printHello = putStrLn "Hello"


vec2DLen :: (Double, Double) -> Double
vec2DLen (x, y) = sqrt(x^2 + y^2)

vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x,y,z) = sqrt( x^2 + y^2 + z^2)

swap :: (Int, Char) -> (Char, Int)
swap (varint, varchar) = (varchar, varint)


threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x, y, z) = if x == y && y == z then True else False

triangleArea :: (Double, Double, Double) -> Double
triangleArea (a, b, c) = sqrt(0.5*(a+b+c) * ((0.5*(a+b+c))-a)*((0.5*(a+b+c))-b)*((0.5*(a+b+c))-c))


sgn :: Int -> Int
sgn n
    | n < 0 = -1
    | n == 0 = 0
    | n > 0 = 1

sgn2 :: Int -> Int
sgn2 n = if n<0 then -1
    else if n==0 then 0
    else 1


absInt :: Int -> Int
absInt x =
    if x>0 then x
    else x*(-1)


min2Int :: (Int, Int) -> Int
min2Int (a, b) =
    if a<b then a
    else b


min3Int :: (Int, Int, Int) -> Int
min3Int (a, b, c) =
    if a<b && a<c then a
    else if b<a && b<c then b
    else c

min3Int2:: (Int, Int, Int) -> Int
min3Int2 (a, b, c) =
    if c<d then c
    else d
    where d = min2Int(a,b)


toUpper :: Char -> Char
toUpper c =
    toEnum(fromEnum(c)-32)

toLower :: Char -> Char
toLower c =
    toEnum(fromEnum c +32)


isDigit :: Char -> Bool
isDigit c =
    fromEnum c > 47 && fromEnum c < 58


charToNum :: Char -> Int
charToNum c =
    if isDigit c then fromEnum c - 48
    else 0

romanDigit :: Char -> String
romanDigit c
    | charToNum c == 1 = "I"
    | charToNum c == 2 = "II"
    | charToNum c == 3 = "III"
    | charToNum c == 4 = "IV"
    | charToNum c == 5 = "V"
    | charToNum c == 6 = "VI"
    | charToNum c == 7 = "VII"
    | charToNum c == 8 = "VIII"
    | charToNum c == 9 = "IX"

min3IntG :: (Int, Int, Int) -> Int
min3IntG (a,b,c)
    | a<b && a<c = a
    | b<a && b<c = b
    | otherwise = c


not' :: Bool -> Bool
not' True = False
not' False = True


isItTheAnswer :: String -> Bool
isItTheAnswer "Love" = True -- :)
isItTheAnswer _      = False


or' :: (Bool, Bool) -> Bool
or' (a, b) =
    if a == True then True
    else if b == True then True
    else False


and' :: (Bool, Bool) -> Bool
and' (a, b)
    | a && b = True
    | otherwise = False

nand' :: (Bool, Bool) -> Bool
nand' (a, b)
    | not a && not b = True
    | otherwise = False

xor' :: (Bool, Bool) -> Bool
xor' (a, b)
    | a && not b = True
    | not a && b = True
    | otherwise = False



not'2 :: Bool -> Bool
not'2 b = case b of
          True  -> False
          False -> True


absInt2 :: Int -> Int 
absInt2 n =
    case (n >= 0) of
        True -> n
        _    -> -n

isItTheAnswer2 :: String -> Bool
isItTheAnswer2 s = case s of
                    "42"    -> True
                    "Love"  -> True
                    _       -> False


roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ( (-1*b-d)/e, (-1*b+d)/e )
   where d = sqrt ((b * b) - (4 * a * c))
         e = 2 * a

unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (x, y) = (x/a, y/a)
    where a = vec2DLen(x,y)


triangleArea2 :: (Double, Double, Double) -> Double
triangleArea2 (a, b, c) = sqrt(p*(p-a)*(p-b)*(p-c))
    where p = 0.5*(a+b+c)



triangleArea3 :: (Double, Double, Double) -> Double
triangleArea3 (a, b, c) =
    let p = 0.5*(a+b+c)
    in sqrt(p*(p-a)*(p-b)*(p-c))

-- tak piszemy komentarze

roots3 :: (Double, Double, Double) -> (Double, Double)
roots3 (a, b, c) = ( (-b - d) / e, (-b + d) / e )
   where 
       {    
        d = sqrt (b * b - 4 * a * c);
        e = 2*a;
       }

