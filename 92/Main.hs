
module Main where

import NumberMultiset
import Data.Maybe (fromJust)
import Control.Monad.State

main = do
  putStrLn $ show $ runState (generateTotals 6) (exampleMS, 1)
