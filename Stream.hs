{-# LANGUAGE ExistentialQuantification #-}

module Stream where

import List

data Stream a = forall s . Stream (s -> Step s a) s

data Step s a = Done
              | Skip s a
              | Yield s a

fromList :: List a -> Stream a
fromList xs = Stream next xs where
  next Empty = Done
  next (Cons p ps) = Yield ps p

toList :: Stream a -> List a
toList (Stream next s) = go s
  where
    go s = case next s of
      Done -> Empty
      Skip s' _ -> go s'
      Yield s' p -> Cons p (go s')

smap :: (a -> b) -> Stream a -> Stream b
smap g (Stream next s) = Stream next' s where
  next' s = case next s of
    Done -> Done
    Skip s' p -> Skip s' (g p)
    Yield s' p -> Yield s' (g p)
    
sfoldl :: (a -> b -> a) -> a -> Stream b -> a
sfoldl g v0 (Stream next s) = go v0 s where
  go acc s = case next s of
    Done -> acc
    Skip s' _ -> go acc s'
    Yield s' v -> go (g acc v) s'