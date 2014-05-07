
-- ghc --make -O2 Main.hs
--
-- time ./Main
-- ./Main  0.04s user 0.00s system 96% cpu 0.051 total
--

module Main where

import NumberMultiset
import Data.Maybe (fromJust)
import Control.Monad.State

main = do
  putStrLn $ show $ runState (generateTotals 6) (exampleMS, 1)
