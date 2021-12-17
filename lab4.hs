-- KARTKÓWKA
-- zadanie 1:   definicja struktury, i obliczyć co będzie(?)
-- zadanie 2:   definicja typu, uzupełnić kawałki instancji definicji klasy
-- zadanie 3:   *nie usłyszałem sadge*


polarToCartesian :: Floating a => (a,a) -> (a,a)
polarToCartesian (r,phi) = (r * cos phi, r * sin phi)

type CartesianCoord' a = (a,a)
type PolarCoord' a = (a,a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r,phi) = (r * cos phi, r * sin phi)



newtype CartesianCoord'' a = MkCartesianCoord'' (a,a)
newtype PolarCoord'' a = MkPolarCoord'' (a,a)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r,phi)) = MkCartesianCoord'' (r * cos phi, r * sin phi)


personInfoToString :: (String,String,String) -> String
personInfoToString (nm,snm,addr) =
 "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr


type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType' = PersonInfo' -> String


personInfoToString' :: PersonInfoToStringType'
personInfoToString' (name,sur,add) = "name:" ++ name ++ ", surname:" ++ sur ++ ", address:" ++ add


-- product type example (one constructor)
data CartInt2DVec = MkCartInt2DVec Int Int -- konwencja: prefix 'Mk' dla konstruktora

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y



data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y



data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

-- xCoord'' :: Cart2DVec'' a -> a
-- xCoord'' (MkCart2DVec'' {x = xVal, y = _}) = xVal

-- yCoord'' :: Cart2DVec'' a -> a
-- yCoord'' (MkCart2DVec'' {y = yVal, x = _}) = yVal -- uwaga na kolejność x,y


-- sum type example (two constructors)
data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x



-- enum type example (special case of sum type)
data ThreeColors = Blue |
                   White |
                   Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"




-- Zdefiniować funkcje dostępowe np. xCoord3D itd. dla następującego 
-- typu (reprezentacja wektorów w 3D, współrzędne kartezjańskie)




data Shape = Circle Float |
             Rectangle Float Float


area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rectangle a b) = a * b



data TrafficLights = RedL | YellowL | GreenL

actionFor :: TrafficLights -> String
actionFor RedL = "Stop"
actionFor YellowL = "Prepare to drive"
actionFor GreenL = "Drive"


data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt




data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"



-- Napisać definicje następujących funkcji:

depthOfBT :: BinTree a -> Int -- głębokość drzewa binarnego
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n l r) = max (1 + depthOfBT l) (1 + depthOfBT r)


-- flattenBTPre :: BinTree a -> [a]  -- napisać trzy wersje: preorder, inorder, postorder
-- flattenBTPre EmptyBT = []
-- flattenBTPre (NodeBT n l r) = 

flattenBTPre :: BinTree a -> [a]
flattenBTPre EmptyBT = []
flattenBTPre (NodeBT n l r ) = [n] ++ flattenBTPre l ++  flattenBTPre r

flattenBTPost :: BinTree a -> [a]
flattenBTPost EmptyBT = []
flattenBTPost (NodeBT n l r ) = flattenBTPre l  ++ flattenBTPre r ++ [n]

flattenBTIn :: BinTree a -> [a]
flattenBTIn EmptyBT = []
flattenBTIn (NodeBT n l r ) = flattenBTPre l ++ [n] ++ flattenBTPre r

mapBT :: (a -> b) -> BinTree a -> BinTree b -- funkcja map dla drzewa binarnego
mapBT f EmptyBT = EmptyBT
mapBT f (NodeBT n l r) = NodeBT (f n) (mapBT f l) (mapBT f r)


insert :: Ord a => a -> BinTree a -> BinTree a -- insert element into BinTree
insert el EmptyBT = NodeBT el EmptyBT EmptyBT
insert el (NodeBT n l r)    | el > n = NodeBT n l (insert el r)
                            | el < n = NodeBT n (insert el l) r
                            | otherwise = NodeBT n l r


newtype MyInt = MkMyInt Int
instance Eq MyInt where
  (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2
instance Ord MyInt where
  (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2

instance Num MyInt where
  (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
  (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
  (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
  negate (MkMyInt i)            = MkMyInt (negate i)
  abs (MkMyInt i)               = MkMyInt (abs i)
  signum (MkMyInt i)            = MkMyInt (signum i)
  fromInteger int               = MkMyInt (fromIntegral int)

instance Show MyInt where
  show (MkMyInt i) = "MkMyInt " ++ show i

instance Eq a => Eq (BinTree a) where
    (==) (NodeBT n l r) (NodeBT n2 l2 r2) = n==n2 && l==l2  && r==r2
    (==) (EmptyBT) (EmptyBT) = True



-- ćwiczenia na kartkówke

data PersonID = PersonID {first :: String, second :: String} 

newtype Box a = MkBox { valueInside :: Int}
instance Show a => Show (Box a) where
    show (MkBox {valueInside = v}) = "Box with " ++ show v


data Tree a = Node (Tree a) a (Tree a) | Leaf

sumSq :: Num p => Tree p -> p
sumSq Leaf = 0
sumSq (Node left x right) = x^2 + sumSq left + sumSq right