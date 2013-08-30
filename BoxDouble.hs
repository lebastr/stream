{-# LANGUAGE BangPatterns #-}


import qualified Data.Vector.Fusion.Stream as S
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G
import Data.Vector.Fusion.Stream (Step(..))
import Data.Vector.Fusion.Util (Id(..))
import Data.Vector.Fusion.Stream.Size (Size(..))
import Data.Vector.Fusion.Stream.Monadic (Stream(..))



nats :: Int -> S.Stream Int
nats n = Stream next 1 size where
  size = Exact n
  next !p = case p <= n of
    True -> Id $ Yield p (p+1)
    False -> Id Done

{-# INLINE nats #-}

enumerate vec = S.zip (nats len) $ G.stream vec where
  len = G.length vec

{-# INLINE enumerate #-}

main = do
  let vec = U.replicate 100000000 1 :: U.Vector Double
  print $ vec G.! 5
  let stream = enumerate vec
  let stream' = S.map (\(i,v) -> v / fromIntegral i) stream
      s = S.foldl' (+) 0 $ S.zipWith (*) stream' (S.tail stream')
  print s
