{-
  (C) 2018 David Lettier
  lettier.com
  LDA Topic Modeling
-}

module ArrayUtils where

import Prelude
import Data.Monoid
import Data.Functor as Functor
import Data.Int
import Data.Map as Map
import Data.Tuple
import Data.Maybe
import Data.Array as Array
import Data.List as List
import Data.Foldable

intToRange :: Int -> Array Int
intToRange i =
  if i <= 0
    then []
    else Array.range 0 (i - 1)

arrayToRange :: forall a. Array a -> Array Int
arrayToRange [] = []
arrayToRange array = Array.range 0 ((Array.length array) - 1)

zipArrayWithIndices :: forall a. Array a -> Array (Tuple a Int)
zipArrayWithIndices [] = []
zipArrayWithIndices array = Array.zip array (arrayToRange array)

arrayWithZeros :: Int -> Array Int
arrayWithZeros size =
  if size <= 0
    then []
    else map (const 0) (Array.range 0 (size - 1))

newIntMatrix :: Int -> Int -> Array (Array Int)
newIntMatrix rows cols =
  foldl
    (\ xs _ -> Array.cons (arrayWithZeros cols) xs)
    []
    (Array.range 0 (rows - 1))

valueAtRow :: forall a. a -> Array a -> Int -> a
valueAtRow default row rowI =
  case Array.index row rowI of
    Nothing -> default
    Just v  -> v

valueAtRowColumn :: forall a. a -> Array (Array a) -> Int -> Int -> a
valueAtRowColumn default matrix rowI colJ =
  case Array.index matrix rowI of
    Nothing  -> default
    Just row -> valueAtRow default row colJ

modifyValueAtIndex :: forall a. Array a -> Int -> (a -> a) -> Array a
modifyValueAtIndex [] _ _ = []
modifyValueAtIndex array index f =
  case Array.index array index of
    Nothing -> array
    Just x  ->
      foldr
      (\ (Tuple e i) es ->
        if i == index
          then Array.cons (f e) es
          else Array.cons e es
      ) [] (zipArrayWithIndices array)

modifyValueAtRowCol
  ::  forall a
  .   Array (Array a)
  ->  Int
  ->  Int
  ->  (a -> a)
  ->  Array (Array a)
modifyValueAtRowCol [] _ _ _ = []
modifyValueAtRowCol matrix row col f =
  case Array.index matrix row of
    Nothing -> matrix
    Just _ ->
      foldr
        (\ (Tuple e i) es ->
          if i == row
            then Array.cons (modifyValueAtIndex e col f) es
            else Array.cons e es
        ) [] (zipArrayWithIndices matrix)

combine :: forall a. Monoid a => Array a -> a
combine a = foldr (\ x xs -> append x xs) mempty a

combineWith :: forall a. Monoid a => a -> Array a -> a
combineWith sep a =
  combine $
    foldr
      (\ (Tuple x i) xs ->
        if i == 0
          then Array.cons x xs
          else Array.cons sep (Array.cons x xs)
      ) [] (zipArrayWithIndices a)

addElementM :: forall a b. Monad b => Array a -> b a -> b (Array a)
addElementM array = Functor.map ((flip Array.cons) array)

addIfNotElement :: forall a. Eq a => a -> Array a -> Array a
addIfNotElement e a = if isNothing $ Array.elemIndex e a then Array.cons e a else a

uniqueArray :: forall a. Eq a => Ord a => Array a -> Array a
uniqueArray =
  List.toUnfoldable <<<
    Map.keys <<<
      foldl (\ xs x -> Map.insert x 1 xs) Map.empty

columnSum :: Array (Array Int) -> Int -> Number
columnSum array i =
  foldl
    (\ s row ->
      case Array.index row i of
        Nothing -> s
        Just v  -> s + (toNumber v)
    )
    0.0
    array

cumulativeSum :: Array Number -> Array Number
cumulativeSum []  = []
cumulativeSum [a] = [a]
cumulativeSum a =
  foldl
    (\ xs i ->
      append
        xs
        [((valueAt a i) + (valueAt xs (i - 1)))]
    ) [valueAt a 0] (Array.range 1 (length a - 1))
  where
    valueAt :: Array Number -> Int -> Number
    valueAt array i =
      case Array.index array i of
        Nothing -> 0.0
        Just v  -> v

transpose :: forall a. a -> Array (Array a) -> Array (Array a)
transpose _     []     = []
transpose empty matrix =
  foldr
    (\ rowI rows ->
      Array.cons
        (
          foldr
            (\ colJ row ->
              case Array.index matrix colJ of
                Nothing     -> Array.cons empty row
                Just oldRow ->
                  case Array.index oldRow rowI of
                    Nothing -> Array.cons empty row
                    Just x  -> Array.cons x row
            )
            []
            (Array.range 0 (rowCount - 1))
        )
        rows
    )
    []
    (Array.range 0 (colCount - 1))
  where
    rowCount :: Int
    rowCount = Array.length matrix
    colCount :: Int
    colCount =
      case Array.index matrix 0 of
        Nothing  -> 0
        Just row -> Array.length row

intRowToNumberRow :: Array Int -> Array Number
intRowToNumberRow = map toNumber

intMatrixToNumberMatrix :: Array (Array Int) -> Array (Array Number)
intMatrixToNumberMatrix = map intRowToNumberRow

addToRow :: Number -> Array Number -> Array Number
addToRow constant = map ((+) constant)

addToAllRows :: Number -> Array (Array Number) -> Array (Array Number)
addToAllRows constant = map (addToRow constant)

rowSums :: Array (Array Number) -> Array Number
rowSums = map sum
