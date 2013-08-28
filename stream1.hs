module Main where

import Stream

main = print $ sfoldl (+) 0 $ smap (*2) $ snats 1000000
