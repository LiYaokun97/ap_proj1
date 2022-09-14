module Warmup where

{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...

type Pos = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y + 1)
move West  (x,y) = (x - 1, y)
move East (x,y) = (x + 1, y)
move South (x,y) = (x, y-1)


moves :: [Direction] -> Pos -> Pos
moves [] (x,y) = (x,y)
moves xs p = foldl (flip move) p xs

data Nat = Zero | Succ Nat
  deriving (Eq, Show, Read, Ord)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat x| x < 0 = Zero
int2nat x| x > 0 = Succ (int2nat(x-1))

add :: Nat -> Nat -> Nat
add x Zero = x
add Zero x = x
add (Succ x) (Succ y) =  Succ (add x (Succ y))

mult :: Nat -> Nat -> Nat
mult _ Zero = Zero
mult Zero _ = Zero
mult (Succ Zero) x = x
mult x (Succ Zero)= x
mult (Succ x) (Succ y) = add (Succ x) (mult (Succ x) y)

data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree
insert a Leaf = Node a Leaf Leaf
insert a (Node n left right)
  | a == n = Node a left right
  | a <  n = Node n (insert a left) right
  | a >  n = Node n left (insert a right)

-- The polymorphic variant, to avoid name clashes with the above
data PTree a = PLeaf | PNode a (PTree a) (PTree a)
  deriving (Eq, Show, Read, Ord)

pinsert :: (Ord a) => a -> PTree a -> PTree a
pinsert a PLeaf = PNode a PLeaf PLeaf
pinsert a (PNode n left right)
  | a == n = PNode a left right
  | a <  n = PNode n (pinsert a left) right
  | a >  n = PNode n left (pinsert a right)

