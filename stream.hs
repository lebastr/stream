module Main where

import Stream
import List

main = print $ sfoldl (+) 0 $ smap (*2) $ fromList $ nats 1000000
