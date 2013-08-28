module Main where

import List

main = print $ lfoldl (+) 0 $ lmap (*2) $ nats 1000000