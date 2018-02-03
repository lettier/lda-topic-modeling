{-
  (C) 2018 David Lettier
  lettier.com
  LDA Topic Modeling
-}

module Stats where

import Prelude
import Data.Maybe
import Data.Tuple
import Data.Int
import Data.Array as Array
import Data.Foldable
import Data.Decimal as Dec
import Control.Monad.Eff
import Control.Monad.Eff.Random

import ArrayUtils

uniformSample :: forall e a. Eq a => Array a -> Eff (random :: RANDOM | e) (Maybe a)
uniformSample [] = pure Nothing
uniformSample [a] = pure (Just a)
uniformSample choices = do
  r <- random
  let d = 1.0 / (toNumber (Array.length choices))
  let choice = foldl (\ prev cur -> currentOrPrevious prev cur r d) (Array.index choices 0) choices
  pure choice
  where
    currentOrPrevious prev cur r d = if isChoice r d (index cur) then (Just cur) else prev
    isChoice r d i = r >= (i * d) && r < ((i + 1.0) * d)
    index e = toNumber $ fromMaybe 0 $ Array.elemIndex e choices

multinomialSample
  ::  forall e a
  .   Array a
  ->  Array Number
  ->  Int
  ->  Eff (random :: RANDOM | e) (Array (Maybe a))
multinomialSample []      _             _ = pure []
multinomialSample _       []            _ = pure []
multinomialSample choices probabilities numberOfTrials = do
  let k = Array.length choices
  let j = Array.length probabilities
  let probabilitiesSum = sum probabilities
  let sumsToOne = probabilitiesSum >= 0.95 && probabilitiesSum <= 1.05
  let cumulativeSum' = cumulativeSum probabilities
  if j /= k || (not sumsToOne) || numberOfTrials <= 0
    then pure []
    else do
      rs <-
        Array.foldM
          (\ rs _ ->
            addElementM rs random
          ) [] (Array.range 0 (numberOfTrials - 1))
      Array.foldM
        (\ trials r ->
          pure (
            Array.cons
              (oneTrial cumulativeSum' r)
              trials
          )
        ) [] rs
  where
    oneTrial :: Array Number -> Number -> Maybe a
    oneTrial cumulativeSum' r =
      case maybeIndex cumulativeSum' r of
        Nothing -> Nothing
        Just i  -> Array.index choices i
    maybeIndex :: Array Number -> Number -> Maybe Int
    maybeIndex cumulativeSum' r =
      foldl
        (\ c (Tuple s i) ->
          case c of
            Nothing -> if r <= s then (Just i) else Nothing
            _       -> c
        ) Nothing (zipArrayWithIndices cumulativeSum')

normalSample
  ::  forall e
  .   Number
  ->  Number
  ->  Eff (random :: RANDOM | e ) Number
normalSample mean std = do
  (Tuple u v) <- genUV
  let u' = Dec.fromNumber u
  let v' = Dec.fromNumber v
  let mean' = Dec.fromNumber mean
  let std' = Dec.fromNumber std
  let s = u' * u' + v' * v'
  let n = Dec.fromNumber (-2.0) * Dec.ln s
  let r = n / s
  let sqrt' = Dec.sqrt r
  let z = u' * sqrt'
  let result = z * std' + mean'
  pure (Dec.toNumber result)
  where
    genUV :: Eff (random :: RANDOM | e) (Tuple Number Number)
    genUV = do
      u0 <- ((-) 1.0 <<< (*) 2.0) <$> random
      v0 <- ((-) 1.0 <<< (*) 2.0) <$> random
      let s = u0 * u0 + v0 * v0
      if s == 0.0 || s >= 1.0
        then genUV
        else pure (Tuple u0 v0)

gammaSample
  ::  forall e
  .   Number
  ->  Number
  ->  Eff (random :: RANDOM | e ) (Maybe Number)
gammaSample alpha beta
  | alpha <= 0.0 || beta <= 0.0 = pure Nothing
  | otherwise = do
    let alpha' = Dec.fromNumber (if alpha > 1.0 then alpha else alpha + 1.0)
    let beta' = Dec.fromNumber (if beta == 1.0 then beta else 1.0)
    let d = alpha' - (Dec.fromNumber 1.0 / Dec.fromNumber 3.0)
    let c = Dec.fromNumber 1.0 / (Dec.sqrt (Dec.fromNumber 9.0 * d))
    uniform <- Dec.fromNumber <$> random
    sample <- gammaSample' d c
    let sample' =
          if alpha <= 1.0
            then sample * (Dec.pow uniform (Dec.fromNumber 1.0 / Dec.fromNumber alpha))
            else sample
    let sample'' =
          if beta == 1.0
            then sample'
            else sample' / Dec.fromNumber beta
    pure $ Just $ Dec.toNumber sample''
    where
      v :: Dec.Decimal -> Dec.Decimal -> Dec.Decimal
      v normal c = Dec.pow ((Dec.fromNumber 1.0) + c * normal) (Dec.fromNumber 3.0)
      test :: Dec.Decimal -> Dec.Decimal -> Dec.Decimal -> Dec.Decimal -> Boolean
      test normal uniform d c =
          (normal > (Dec.fromNumber (-1.0)) / c)
        &&
          (
              (Dec.ln uniform)
            < (
                  ((Dec.fromNumber 1.0) / ((Dec.fromNumber 2.0) * (normal * normal)))
                + d
                - d
                * (v normal c)
                + d
                * (Dec.ln (v normal c))
              )
          )
      gammaSample'
        ::  Dec.Decimal
        ->  Dec.Decimal
        ->  Eff (random :: RANDOM | e) Dec.Decimal
      gammaSample' d c = do
        normal <- Dec.fromNumber <$> normalSample 0.0 1.0
        uniform <- Dec.fromNumber <$> random
        if test normal uniform d c
          then pure (d * (v normal c))
          else gammaSample' d c

dirichletSample
  ::  forall e
  .   Array Number
  ->  Eff (random :: RANDOM | e) (Maybe (Array Number))
dirichletSample alphas = do
  let hasLteZero = foldl (\ xs alpha -> xs || alpha <= 0.0) false alphas
  if hasLteZero
    then pure Nothing
    else do
      maybeSamples <-
        Array.foldM
          (\ xs alpha -> (flip Array.cons xs) <$> gammaSample alpha 1.0)
          []
          alphas
      let hasNothing = foldl (\ xs m -> xs || isNothing m) false maybeSamples
      if hasNothing
        then pure Nothing
        else do
          let samples = foldl (\ xs x -> Array.cons (fromMaybe 0.0 x) xs) [] maybeSamples
          let sampleSum = sum samples
          if sampleSum == 0.0
            then pure Nothing
            else pure $ Just $ map (flip (/) sampleSum) samples

occurrenceFrequencies
  ::  forall a
  .   Eq a
  =>  Array a
  ->  Array (Maybe a)
  ->  Array Number
occurrenceFrequencies types maybeSamples =
  foldr (\ c xs -> Array.cons (toNumber c / toNumber countsSum) xs) [] counts
  where
    countsSum :: Int
    countsSum = sum counts
    counts :: Array Int
    counts = foldl modifyCount (Array.replicate (length types) 0) samples
    samples :: Array a
    samples = foldl (\ xs x ->
                case x of
                  Nothing -> xs
                  Just x' -> Array.cons x' xs
              ) [] maybeSamples
    modifyCount :: Array Int -> a -> Array Int
    modifyCount xs a =
      case (Array.elemIndex a types) of
        Nothing -> xs
        Just i  -> case (Array.modifyAt i ((+) 1) xs) of
                    Nothing  -> xs
                    Just xs' -> xs'

rowWiseRelativeFrequencies :: Array (Array Number) -> Array (Array Number)
rowWiseRelativeFrequencies []     = []
rowWiseRelativeFrequencies matrix =
  map
    (\ (Tuple row i) ->
      map
        (\ x ->
          case Array.index sums i of
            Nothing -> 0.0
            Just s  ->
              if s == 0.0
                then 0.0
                else x / s
        )
        row
    )
    (zipArrayWithIndices matrix)
  where
    sums :: Array Number
    sums = rowSums matrix
