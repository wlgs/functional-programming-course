-- import System.Environment
-- import System.IO
-- import System.Environment
{-# LANGUAGE DeriveFunctor #-}

actSeq = putChar 'A' >> putChar 'G' >> putChar 'H' >>  putChar '\n'

doActSeq :: IO ()
doActSeq = do
  putChar 'A'
  putChar 'G'
  putChar 'H'
  putChar '\n'


echo1 :: IO ()
echo1 = getLine >>= putStrLn

doEcho1 :: IO ()
doEcho1 = do
  line <- getLine
  putStrLn line



echo2 :: IO ()
echo2 = getLine >>= \line -> putStrLn $ line ++ "!"

doEcho2 :: IO ()
doEcho2 = do
  line <- getLine
  putStrLn $ line ++ "!"
  



echo3 :: IO ()
echo3 =  getLine >>= \l1 -> getLine >>= \l2 -> putStrLn $ l1 ++ l2

dialog :: IO ()
dialog = putStr "What is your happy number? "
         >> getLine
         >>= \n -> let num = read n :: Int in
                   if num == 7
                   then putStrLn "Ah, lucky 7!"
                   else if odd num
                        then putStrLn "Odd number! That's most people's choice..."
                        else putStrLn "Hm, even number? Unusual!"



-- Zadania:
-- Napisać odpowiedniki echo3 i dialog wykorzystujące notację do
-- Napisać odpowiednik twoQuestions bez użycia notacji do

twoQuestions :: IO ()
twoQuestions = do
  putStr "What is your name? "
  name <- getLine
  putStr "How old are you? "
  age <- getLine
  print (name,age)     


-- twoQuestions' :: IO ()
-- twoQuestions' = putStrLn "What is your name?" 
--             >>= \n -> getLine
--             >> putStr "How old are you?" 
--             >>= \a -> getLine
--             >> 


-- ^ TODO THE FUNCITONS



nTimes :: Int -> IO () -> IO ()
nTimes 0 action = return ()
nTimes n action = do
  action
  nTimes (n-1) action

ioActionFactory :: Int -> String -> IO ()
ioActionFactory n = case n of
  1 -> \name -> putStrLn ("Good morning, " ++ name)
  2 -> \name -> putStrLn ("Good afternoon, " ++ name)
  3 -> \name -> putStrLn ("Good night, " ++ name)
  _ -> \name -> putStrLn ("Hello, " ++ name)

actionList :: [IO ()]
actionList = [ioActionFactory 1 "Ben",
              ioActionFactory 2 "Joe",
              ioActionFactory 3 "Ally"]

sequence'        :: [IO ()] -> IO ()
sequence' []     =  return ()
sequence' (a:as) =  do a
                       sequence' as



-- Zadania:
-- Napisać odpowiednik sequence' wykorzystujący foldr
-- Zmienić postać 1. agrumentu foldr: z >> na wyrażenie lambda
-- Napisać odpowiednik sequence' wykonujący ‘akcje’ od ostaniej do pierwszej; rozważyć co najmniej dwa warianty, np. foldr na odwróconej liście i wykorzystanie foldl





-- TODO ^^ WRITE FUNCTIONS



-- Ćwiczenie opcjonalne! \/

-- main = do
--   (inFileName:outFileName:_) <- getArgs
--   inHdlr <- openFile inFileName ReadMode
--   outHdlr <- openFile outFileName WriteMode
--   inpStr <- hGetContents inHdlr
--   hPutStr outHdlr inpStr
--   hClose inHdlr
--   hClose outHdlr


-- main = do
--   (inFileName:outFileName:_) <- getArgs
--   inpStr <- readFile inFileName
--   writeFile outFileName inpStr

-- Ćwiczenie opcjonalne end /\

data MyList a = EmptyList
              | Cons a (MyList a) deriving Show

instance Functor MyList where
  fmap _ EmptyList    = EmptyList
  fmap f (Cons x mxs) = Cons (f x) (fmap f mxs)




-- Sprawdzić możliwość automatycznego wygenerowania instancji Functor dla typu MyList (klauzula deriving)
-- Napisać własną implementację funktora (instance Functor), a następnie sprawdzić możliwość jej automatycznego wygenerowania dla drzewa binarnego zdefiniowanego jako


data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show)





-- TODO^ WRITE FUNCITONS




newtype Box a = MkBox a deriving Show
instance Functor Box where
  fmap f (MkBox x) = MkBox (f x)
instance Applicative Box where
  pure = MkBox
  (MkBox f) <*> w = fmap f w


-- Napisać implementacje funktora aplikatywnego (instance Applicative) dla typu

newtype MyTriple a = MyTriple (a,a,a) deriving Show





-- TODO ^^ WRITE FUNCTIONS






-- trening do kartkowki:
test :: p -> IO ()
test s = getLine >>= \s -> return 3 >>= \n -> putStrLn $ show n ++ s

data Tree a = Node a (Tree a) (Tree a)
            | Leaf

pathsSum:: Num a => Tree a -> [a]
pathsSum Leaf = pure 0
pathsSum (Node a lt rt) = concat $ ([(a +)] <*>) <$> (fmap pathsSum [lt,rt])

paths:: Tree a -> [[a]]
paths Leaf = pure []
paths (Node a lt rt) = concat $ ([(a :)] <*>) <$> (fmap paths [lt,rt])

