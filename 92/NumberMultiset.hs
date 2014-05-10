
-- This module contains a generator for solving Euler problem 92 using multisets in about 0.04s

-- A number multiset can be considered the multiset (set incl. dupicates) of digits 1-9 over the
-- size of the set eg.
-- 0 0 0 0 0 0 1
-- 0 0 0 0 0 0 2
-- 0 0 0 0 0 0 ...
-- 0 0 0 0 0 1 1
-- etc.
--
-- Each of these numbers will either terminate at 89 or not.
-- Each of these numbers will then have a number of permutations which also terminate at 89.
-- We can calculate how many permutations there are for each qualifying value using multinomial coefficients

module NumberMultiset where

import Control.Monad.State
import Data.Vector
import Data.Digits
import Data.Maybe (fromJust)
import Prelude hiding (take, (++), replicate, length, head, init, last, drop)

type NumberMultiset = Vector Int

digitLimit :: Int
digitLimit = 9

equalizeNumberMultiset :: NumberMultiset -> Int -> NumberMultiset
equalizeNumberMultiset m i = (take i m) ++ (replicate (size - i) value)
  where size = length m
        value = m ! i

incrementPosition :: NumberMultiset -> Int -> NumberMultiset
incrementPosition m i = (take i m) ++ (singleton $ value + 1) ++ (drop (i + 1) m)
  where value = m ! i

incrementNumberMultiset :: NumberMultiset -> Int -> (Maybe NumberMultiset, Int)
incrementNumberMultiset m i
  | head m == digitLimit = (Nothing, i)
  | (i == (size - 1)) && (lastElement < digitLimit) = (Just $ incrementPosition m i, i)
  | m ! i  == digitLimit = incrementNumberMultiset m (i - 1) 
  | otherwise = (Just (equalizeNumberMultiset (incrementPosition m i) i), (size - 1))

  where size = length m
        lastElement = last m

exampleMS :: NumberMultiset
exampleMS = fromList $ [0,0,0,0,0,0,1]

-----------------------------------------

type PermutationSumValue = Int
type PermutationSumState = (NumberMultiset, Int)

squareDigits = Data.Vector.map (^ 2)
multisetToNumber m = Data.Vector.ifoldl (\acc i val -> (10 ^ (power i) * val) + acc) 0 m
  where size = length m
        power :: Int -> Int
        power i = size - i -1

squareTerminator :: Int -> Int
squareTerminator x
  | x == 1 = 1
  | x == 89 = 89
  | otherwise = squareTerminator $ square digitForm
  where square number = Prelude.sum $ Prelude.map (^ 2) number
        digitForm   = digits 10 x

multisetToDigitCount :: NumberMultiset -> NumberMultiset
multisetToDigitCount = Data.Vector.foldl (\acc val -> acc // [(val, (acc ! val) + 1)]) countVector
  where countVector = replicate (digitLimit + 1) 0

multinomalCoefficient :: NumberMultiset -> Int
multinomalCoefficient m = (factorial (length m)) `div` (Data.Vector.product (Data.Vector.map factorial countVector)) 
  where countVector = multisetToDigitCount m
        factorial n = Prelude.product [1..n]

calculatePermutationTotal  :: NumberMultiset -> Int
calculatePermutationTotal m = case squareTerminator $ multisetToNumber m of
  89 -> multinomalCoefficient m
  _  -> 0

generateTotals :: Int -> State PermutationSumState PermutationSumValue 
generateTotals i = do
  (m, total) <- get
  nextStepPermutation m i total

nextStepPermutation :: NumberMultiset -> Int -> Int -> State PermutationSumState PermutationSumValue
nextStepPermutation m i total = do
  case nextMultiset of
    Nothing  -> return $ total
    Just m'  -> do put (m', total + calculatePermutationTotal m')
                   generateTotals i'
                  
  where (nextMultiset, i') = incrementNumberMultiset m i

