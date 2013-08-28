{-# LANGUAGE BangPatterns #-}

module List where

data List a = Empty | Cons a (List a)

lmap :: (a -> b) -> List a -> List b
lmap _ Empty = Empty
lmap f (Cons x xs) = Cons (f x) (lmap f xs)

lzip :: List a -> List b -> List (a,b)
lzip Empty _ = Empty
lzip (Cons _ _) Empty = Empty
lzip (Cons x xs) (Cons y ys) = Cons (x,y) (lzip xs ys)

ltail :: List a -> List a
ltail (Cons _ xs) = xs

lfoldl :: (a -> b -> a) -> a -> List b -> a
lfoldl g x0 ps = go x0 ps where
  go x0 Empty = x0
  go x0 (Cons p ps) = go (g x0 p) ps

nats :: Int -> List Int
nats n = go 0 where
  go x | x <= n = Cons x $ go (x+1)
       | otherwise = Empty
