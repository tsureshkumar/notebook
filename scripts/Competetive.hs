--- Competetive Programs in Haskell
--

import Data.Array
import Data.ByteString.Char8 qualified as BC
import Data.Function

-- Convert a list to an array
listToArray :: [Int] -> Array Int Int
listToArray a = listArray (0, length a - 1) a


-- Filter an array based on a predicate
filterArray :: (Ix i, Integral e) => (e -> Bool) -> Array i e -> Array i e
filterArray p arr =
  let filtered = [(i, x) | (i, x) <- assocs arr, p x] -- Filtered associations
   in array (fst (head filtered), fst (last filtered)) filtered

-- Map a function over an array
mapArray :: (Ix i, Integral e) => (e -> e) -> Array i e -> Array i e
mapArray f arr =
  let bounds' = bounds arr
      mapped = [(i, f x) | (i, x) <- assocs arr] -- Apply the function
   in array bounds' mapped

-- Read integers from a string
readInts' :: String -> Array Int Int
readInts' input = do
  let a = parseInts . BC.pack $ input
  listToArray a
  where
    parseInts = map (fst . fromJust . BC.readInt) . BC.words
    fromJust (Just x) = x

-- Slice an array between two indices (inclusive)
sliceArray :: (Ix i) => Array i e -> (i, i) -> Array i e
sliceArray arr (low, high) = array (low, high) [(i, arr ! i) | i <- range (low, high)]

readInts :: IO [Int]
readInts = do
  input <- BC.getLine
  return $ parseInts input
  where
    parseInts = map (fst . fromJust . BC.readInt) . BC.words
    fromJust (Just x) = x

main :: IO ()
main = do
  ints <- readInts
  print ints
