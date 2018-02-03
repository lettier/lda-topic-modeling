{-
  (C) 2018 David Lettier
  lettier.com
  LDA Topic Modeling
-}

module DocumentGeneration where

import Prelude
import Data.Maybe
import Data.Foldable
import Data.Traversable
import Data.Array as Array
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.Console

import Stats
import ArrayUtils

generateDocuments
  ::  forall e a
  .   Eq a
  =>  Int
  ->  Array Int
  ->  Array a
  ->  Array String
  ->  Number
  ->  Number
  ->  Eff (random :: RANDOM, console :: CONSOLE | e) (Array String)
generateDocuments
  documentCount
  documentWordCounts
  topics
  vocabulary
  alpha
  beta
  = do
  let documentCountValid = documentCount > 0
  let topicsValid = topicCount > 0
  let vocabularyValid = vocabCount > 0
  let alphaValid = alpha > 0.0
  let betaValid = beta > 0.0
  if
        documentCountValid
    &&  topicsValid
    &&  vocabularyValid
    &&  alphaValid
    &&  betaValid
    then do
      topicVocabMatrix <-
        generateRowColumnMatrix
          topicCount
          vocabCount
          (map (const beta) (intToRange vocabCount))
      documentTopicMatrix <-
        generateRowColumnMatrix
          documentCount
          topicCount
          (map (const alpha) (intToRange topicCount))
      log "generateDocuments: topicVocabMatrix"
      _ <- logShow topicVocabMatrix
      log "-----------------------------------"
      log "generateDocuments: documentTopicMatrix"
      _ <- logShow documentTopicMatrix
      log "-----------------------------------"
      traverse
        (\ i ->
          foldM
            (\ xs _ -> do
              maybeString <- topic documentTopicMatrix i >>= word topicVocabMatrix
              case maybeString of
                Nothing -> pure xs
                Just s  -> pure $ s <> " " <> xs
            )
            ""
            (intToRange $ valueAtRow 0 documentWordCounts i)
        )
        (intToRange documentCount)
    else pure []
  where
    word :: Array (Array Number) -> Maybe Int -> Eff (random :: RANDOM, console :: CONSOLE | e) (Maybe String)
    word _ Nothing  = pure Nothing
    word m (Just i) = do
      result' <- result
      case result' of
        [s] -> pure s
        _   -> pure Nothing
      where
        result :: Eff (random :: RANDOM, console :: CONSOLE | e) (Array (Maybe String))
        result = multinomialSample vocabulary (getRow m i) 1
    topic :: Array (Array Number) -> Int -> Eff (random :: RANDOM, console :: CONSOLE | e) (Maybe Int)
    topic m i = do
      result' <- result
      case result' of
        [(Just t)] -> pure $ Array.elemIndex t topics
        _   -> pure Nothing
      where
        result :: Eff (random :: RANDOM, console :: CONSOLE | e) (Array (Maybe a))
        result = multinomialSample topics (getRow m i) 1
    getRow :: Array (Array Number) -> Int -> Array Number
    getRow matrix i =
      case Array.index matrix i of
        Nothing  -> []
        Just row -> row
    topicCount :: Int
    topicCount = Array.length topics
    vocabCount :: Int
    vocabCount = Array.length vocabulary

generateRowColumnMatrix
  ::  forall e
  .   Int
  ->  Int
  ->  Array Number
  ->  Eff (random :: RANDOM | e) (Array (Array Number))
generateRowColumnMatrix rowCount columnCount alphas = do
  let rowCountValid = rowCount > 0
  let columnCountValid = columnCount > 0
  let alphasValid =
            (foldl (\ xs x -> xs || x > 0.0) false alphas)
        &&  (Array.length alphas == columnCount)
  if rowCountValid && columnCountValid && alphasValid
    then
      foldM
        (\ xs _ -> generateDirichletRow >>= pure <<< (flip Array.cons xs))
        []
        (intToRange rowCount)
    else pure []
  where
    generateDirichletRow :: Eff (random :: RANDOM | e) (Array Number)
    generateDirichletRow = do
      maybeRow <- dirichletSample alphas
      case maybeRow of
        Nothing  -> generateDirichletRow
        Just row -> pure row
