{-# LANGUAGE TypeSynonymInstances #-}

import System.Environment
import Data.Maybe
import Control.Applicative
import qualified Pipes.Prelude as P
import Control.Monad
import Pipes


{-
 Expression Generation Algorithm:

 @Input
   level - level of arithmetic expression
   operands domain : [1..100] of integers
   operators : ['+', '-', '*', '/']


 1) Generate all available schemes of full binary trees for "level"
 2) Generate all combinations of operands  = 100 ^ level 
 3) Generate all combinations of operators =   4 ^ (level-1)
 4) Combine all schemes and combinations to get all expressions


 Bonus-3

 The number of all expressions at "level" is

 (Number of schemes at "level") * (Number of combinations of operands) * (number of combinations of operators )

 To calculate the number of schemes we can use follow recurrent expression

 s(0) = 1
 s(1) = 1
 s(2) = 1

 s(level) = Sum[s(i)*(level-i)], where i from 1 to level-1


-}





main :: IO ()
main = do
     args <- getArgs
     case args of
       ["--level", level] ->  start . fst . head $ reads level 
       _ -> print "Arguments are not recognized" >> return ()


start :: Int -> IO ()
start level = runEffect $ do
  combinations level
  >-> genEqs (genBinaryTrees level)
  >-> P.concat
  >-> P.filter (isJust . evalTree )                   -- | filter out non Integer expressions
  >-> P.filter (\eq -> (fromJust.evalTree $ eq) > 0)  -- | leave only positive expressions 
  >-> printAndCheck


data BOperator = M | D | P | S | None deriving (Eq, Enum);
instance Show BOperator where
  show M = "*"
  show D = "/"
  show S = "-"
  show P = "+"
  show None = ""

  
  
data BTree a = Operand a | Operator BOperator (BTree a) (BTree a)  deriving Show
type Combination = ([Int], [BOperator])


-- | fill out the BTree scheme with combination of operands and operations 
updateTree :: BTree Int -> Combination -> (BTree Int, Combination)
updateTree (Operand _) (a:as,b) = (Operand $ a, (as, b))
updateTree (Operator o (Operand _) (Operand _)) (a1:a2:as, b1:bs) =
  (Operator b1 (Operand a1) (Operand a2), (as, bs))
updateTree (Operator o (Operand _) rtree) (a:as, b) = let
   (right, (a', b':bs')) = updateTree rtree (as,b)
   in (Operator b' (Operand a) right, (a', bs'))
updateTree (Operator o ltree (Operand _)) (a, b) = let
   (left, (a':as', b':bs')) = updateTree ltree (a, b)
   in (Operator b' left (Operand a'), (as', bs'))
updateTree (Operator o ltree rtree) (a, b) = let
   (right, (a', b')) = updateTree rtree (a, b)
   (left, (a'', b'':bs'')) = updateTree ltree (a', b')
   in (Operator b'' left right, (a'', bs''))


-- | eval Int tree expression if possible 
evalTree :: BTree Int -> Maybe Int
evalTree (Operand x) = Just x
evalTree (Operator M l r) = (*) <$> evalTree l <*> evalTree r
evalTree (Operator P l r) = (+) <$> evalTree l <*> evalTree r
evalTree (Operator S l r) = (-) <$> evalTree l <*> evalTree r
evalTree (Operator D l r) = do
  l' <- evalTree l
  r' <- evalTree r
  case (r' == 0 || mod l' r' /= 0) of
    True  ->  Nothing
    False ->  Just $ fromIntegral $ floor $ (fromIntegral l') / (fromIntegral r')


-- | show Expression tree for humans
showTree :: BTree Int -> String
showTree (Operand x) = show  x
showTree (Operator o l r) =
  let left  = if (opPrior o <= getOpPrior l) then (showTree l) else "(" ++  (showTree l) ++ ")"
      right = if or [opPrior o > getOpPrior r, o == getOp r && (not . isCommutative $ o)] then "(" ++  (showTree r) ++ ")" else (showTree r) 
  in left ++ " " ++ (show o) ++ " "  ++ right

 where
   opPrior M = 2
   opPrior D = 2
   opPrior S = 1
   opPrior P = 1
   opPrior None = 3

   getOp :: BTree a -> BOperator
   getOp (Operator o _ _) = o
   getOp _ = None

   getOpPrior :: BTree Int -> Int
   getOpPrior = opPrior . getOp

   -- | is operation commutative
   isCommutative :: BOperator -> Bool
   isCommutative M = True
   isCommutative P = True
   isCommutative _ = False




-- | generate all combination of full binary trees fon N elements
genBinaryTrees :: Int -> [BTree Int]
genBinaryTrees 1 = [Operand 1]
genBinaryTrees n = do
   cmb <- [(a,b) | a <-[1..n-1], b <- [1..n-1], a + b == n]
   tls <- pure $ genBinaryTrees . fst $ cmb
   trs <- pure $ genBinaryTrees . snd $ cmb
   map (\(a,b) -> Operator P a b) [(l,r) | l <- tls, r <- trs]
   



-- | substitute operands and operations to math expression
fillEquation :: Combination -> BTree Int -> BTree Int
fillEquation (a,b) tree = undefined


-- | generate combination of all operands and operations
combinations :: Monad m => Int -> Producer Combination m ()
combinations n = each [(a,b) | a <- allComb n [1..100], b <- allComb (n-1) [M,D,S,P]]
  where
    allComb :: Int -> [a] ->  [[a]]
    allComb n vs = mapM (const vs) [1..n]

-- wait for combination
genEqs :: Monad m => [BTree Int] -> Pipe Combination [BTree Int] m ()
genEqs trees = forever $ do
  c <- await
  yield $ map (fst . flip updateTree c) trees

-- print expression, read user answer and check it
printAndCheck :: Consumer (BTree Int) IO ()
printAndCheck = forever $ do
  eq <- await
  lift . print $ showTree eq
  --lift . print $ eq
  P.stdinLn >-> do
    str <- await
    eval <- lift . return . evalTree $ eq
    lift . print $ if (show . fromJust $ eval) == str then "Correct" else "Incorrect"






