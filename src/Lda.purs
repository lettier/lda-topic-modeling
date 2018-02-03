{-
  (C) 2018 David Lettier
  lettier.com
  LDA Topic Modeling
-}

module Lda where

import Prelude
import Data.Functor as Functor
import Data.Tuple
import Data.Maybe
import Data.Int
import Data.Array as Array
import Data.Foldable
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.Console
import Control.Monad.Aff

import ArrayUtils
import DocumentWord
import Stats

type LdaInput t =
  {
      alpha :: Number
    , beta :: Number
    , topics :: Array t
    , iterations :: Int
    , documents :: Array String
    , vocabulary :: Array String
  }

type LdaOutput t =
  {
      vocabTopicMatrix :: Array (Array Int)
    , documentTopicMatrix :: Array (Array Int)
    , documentWords :: Array (DocumentWord t)
  }

type LdaIterationIo t =
  {
      alpha :: Number
    , beta :: Number
    , topics :: Array t
    , topicCount :: Number
    , vocabCount :: Number
    , vocabTopicMatrix :: Array (Array Int)
    , documentTopicMatrix :: Array (Array Int)
    , documentWords :: Array (DocumentWord t)
    , iteration :: Int
  }

foreign import _performLdaAsyncViaWorker
  ::  forall e t
  .   (LdaInput t)
  ->  (
            LdaOutput t
        ->  Eff e Unit
      )
  ->  Eff e Unit

foreign import _performLdaAsyncViaRAF
  ::  forall e t
  .   (LdaInput t)
  ->  (
            LdaInput t
        ->  Eff (random :: RANDOM, console :: CONSOLE | e)
            (LdaIterationIo t)
      )
  ->  (
            LdaIterationIo t
        ->  Eff (random :: RANDOM, console :: CONSOLE | e)
            (LdaIterationIo t)
      )
  ->  (
            LdaIterationIo t
        ->  LdaOutput t
      )
  ->  (
            LdaOutput t
        ->  Eff e Unit
      )
  ->  Eff e Unit

performLdaAsyncViaWorker
  ::  forall e t
  .   Eq t
  =>  Show t
  =>  LdaInput t
  ->  Aff (random :: RANDOM, console :: CONSOLE | e)
      (LdaOutput t)
performLdaAsyncViaWorker input =
  makeAff $
    \ _ success ->
      _performLdaAsyncViaWorker
        input
        success

performLdaAsyncViaRAF
  ::  forall e t
  .   Eq t
  =>  Show t
  =>  LdaInput t
  ->  Aff (random :: RANDOM, console :: CONSOLE | e)
      (LdaOutput t)
performLdaAsyncViaRAF input =
  makeAff $
    \ _ success ->
      _performLdaAsyncViaRAF
        input
        performLdaInitialization
        performLdaIteration
        ldaIterationOutputToLdaOutput
        success

performLda
  ::  forall e t
  .   Eq t
  =>  Show t
  =>  LdaInput t
  ->  Eff (random :: RANDOM, console :: CONSOLE | e)
      (LdaOutput t)
performLda
  input@{
      iterations
  }
  = do
  init <- performLdaInitialization input
  result <-
    Array.foldM
      (\ r _ -> performLdaIteration r)
      init
      (
        Array.range
          0
          ((if iterations <= 0 then 1 else iterations) - 1)
      )
  pure $
    ldaIterationOutputToLdaOutput result

performLdaInitialization
  ::  forall e t
  .   Eq t
  =>  Show t
  =>  LdaInput t
  ->  Eff (random :: RANDOM, console :: CONSOLE | e)
      (LdaIterationIo t)
performLdaInitialization
  {
      alpha
    , beta
    , topics
    , iterations
    , documents
    , vocabulary
  }
  = do
  let numberOfTopics = length topics
  documentWords <-
    Functor.map Array.concat (
      Array.foldM (\ xs (Tuple document i) ->
        addElementM xs (createDocumentWords topics vocabulary document i)
      ) [] (zipArrayWithIndices documents)
    )
  let vocabTopicMatrix =
        foldl (\ xs (DocumentWord { topicIndex, vocabIndex }) ->
          case vocabIndex of
            Nothing -> xs
            Just v  ->
              case topicIndex of
                Nothing -> xs
                Just t  -> modifyValueAtRowCol xs v t ((+) 1)
        ) (newIntMatrix (Array.length vocabulary) numberOfTopics) documentWords
  let documentTopicMatrix =
        foldl (\ xs (DocumentWord { topicIndex, documentIndex }) ->
          case topicIndex of
            Nothing -> xs
            Just t  -> modifyValueAtRowCol xs documentIndex t ((+) 1)
        ) (newIntMatrix (Array.length documents) numberOfTopics) documentWords
  pure
    {
        alpha: alpha
      , beta: beta
      , topics: topics
      , topicCount: toNumber $ Array.length topics
      , vocabCount: toNumber $ Array.length vocabulary
      , vocabTopicMatrix: vocabTopicMatrix
      , documentTopicMatrix: documentTopicMatrix
      , documentWords: documentWords
      , iteration: 0
    }

ldaIterationOutputToLdaOutput
  ::  forall e t
  .   Eq t
  =>  LdaIterationIo t
  ->  LdaOutput t
ldaIterationOutputToLdaOutput
  {
      vocabTopicMatrix: vocabTopicMatrix
    , documentTopicMatrix: documentTopicMatrix
    , documentWords: documentWords
  }
  =
  {
      vocabTopicMatrix: vocabTopicMatrix
    , documentTopicMatrix: documentTopicMatrix
    , documentWords: documentWords
  }

performLdaIteration
  ::  forall e t
  .   Eq t
  =>  LdaIterationIo t
  ->  Eff (random :: RANDOM, console :: CONSOLE | e)
      (LdaIterationIo t)
performLdaIteration
  {
      alpha
    , beta
    , topics
    , topicCount
    , vocabCount
    , vocabTopicMatrix: vocabTopicMatrix
    , documentTopicMatrix
    , documentWords
    , iteration
  }
  = do
  Array.foldM
    (\
      {
          alpha: alpha'
        , beta: beta'
        , topics: topics'
        , topicCount: topicCount'
        , vocabCount: vocabCount'
        , vocabTopicMatrix: vocabTopicMatrix'
        , documentTopicMatrix: documentTopicMatrix'
        , documentWords: documentWords'
        , iteration: iteration'
      }
      documentWord
      -> do
        {
            vocabTopicMatrix: vocabTopicMatrix''
          , documentTopicMatrix: documentTopicMatrix''
          , documentWord: documentWord'
        } <-
          updateDocumentWordTopic
            {
                alpha: alpha'
              , beta: beta'
              , topics: topics'
              , topicCount: topicCount'
              , vocabCount: vocabCount'
              , vocabTopicMatrix: vocabTopicMatrix'
              , documentTopicMatrix: documentTopicMatrix'
              , documentWord: documentWord
            }
        pure
          {
              alpha: alpha'
            , beta: beta'
            , topics: topics'
            , topicCount: topicCount'
            , vocabCount: vocabCount'
            , vocabTopicMatrix: vocabTopicMatrix''
            , documentTopicMatrix: documentTopicMatrix''
            , documentWords: Array.cons documentWord' documentWords'
            , iteration: iteration'
          }
    )
    {
        alpha: alpha
      , beta: beta
      , topics: topics
      , topicCount: topicCount
      , vocabCount: vocabCount
      , vocabTopicMatrix: vocabTopicMatrix
      , documentTopicMatrix: documentTopicMatrix
      , documentWords: []
      , iteration: iteration + 1
    }
    documentWords

updateDocumentWordTopic
  ::  forall t e
  .   Eq t
  =>  {
          alpha :: Number
        , beta :: Number
        , topics :: Array t
        , topicCount :: Number
        , vocabCount :: Number
        , vocabTopicMatrix :: Array (Array Int)
        , documentTopicMatrix :: Array (Array Int)
        , documentWord :: DocumentWord t
      }
  ->  Eff (random :: RANDOM, console :: CONSOLE | e)
      {
          vocabTopicMatrix :: Array (Array Int)
        , documentTopicMatrix :: Array (Array Int)
        , documentWord :: DocumentWord t
      }
updateDocumentWordTopic
  {
      vocabTopicMatrix: vocabTopicMatrix
    , documentTopicMatrix: documentTopicMatrix
    , documentWord:
      DocumentWord documentWord@{
          topicIndex: Nothing
      }
  }
  = pure
    {
        vocabTopicMatrix: vocabTopicMatrix
      , documentTopicMatrix: documentTopicMatrix
      , documentWord: DocumentWord documentWord
    }
updateDocumentWordTopic
  {
      vocabTopicMatrix: vocabTopicMatrix
    , documentTopicMatrix: documentTopicMatrix
    , documentWord:
      DocumentWord documentWord@{
          vocabIndex: Nothing
      }
  }
  = pure
    {
        vocabTopicMatrix: vocabTopicMatrix
      , documentTopicMatrix: documentTopicMatrix
      , documentWord: DocumentWord documentWord
    }
updateDocumentWordTopic
  {
      alpha: alpha
    , beta: beta
    , topics: topics
    , topicCount: topicCount
    , vocabCount: vocabCount
    , vocabTopicMatrix: vocabTopicMatrix
    , documentTopicMatrix: documentTopicMatrix
    , documentWord:
      DocumentWord documentWord@{
          word: word
        , topicIndex: (Just currentTopicIndex)
        , vocabIndex: (Just vocabIndex)
        , documentWordIndex: documentWordIndex
        , documentIndex: documentIndex
      }
  }
  = do
  let returnOnFailure =
        {
            vocabTopicMatrix: vocabTopicMatrix
          , documentTopicMatrix: documentTopicMatrix
          , documentWord: DocumentWord documentWord
        }
  let vocabTopicMatrix' =
        modifyValueAtRowCol
          vocabTopicMatrix
          vocabIndex
          currentTopicIndex
          (\ c -> if c <= 0 then 0 else c - 1)
  let documentTopicMatrix' =
        modifyValueAtRowCol
          documentTopicMatrix
          documentIndex
          currentTopicIndex
          (\ c -> if c <= 0 then 0 else c - 1)
  let maybeProbabilities =
        documentWordTopicProbablities
          {
              alpha: alpha
            , beta: beta
            , topicCount: topicCount
            , vocabCount: vocabCount
            , vocabTopicMatrix: vocabTopicMatrix'
            , documentTopicMatrix: documentTopicMatrix'
            , documentWord: DocumentWord documentWord
          }
  case maybeProbabilities of
    Nothing -> pure returnOnFailure
    Just probabilities -> do
      newMaybeTopics <- multinomialSample topics probabilities 1
      case newMaybeTopics of
        [(Just newTopic)] -> do
          let maybeNewTopicIndex = Array.elemIndex newTopic topics
          case maybeNewTopicIndex of
            Nothing -> pure returnOnFailure
            Just newTopicIndex -> do
              if newTopicIndex == currentTopicIndex
                then pure returnOnFailure
                else do
                  let vocabTopicMatrix'' =
                        modifyValueAtRowCol
                          vocabTopicMatrix'
                          vocabIndex
                          newTopicIndex
                          ((+) 1)
                  let documentTopicMatrix'' =
                        modifyValueAtRowCol
                          documentTopicMatrix'
                          documentIndex
                          newTopicIndex
                          ((+) 1)
                  pure (
                    {
                        vocabTopicMatrix: vocabTopicMatrix''
                      , documentTopicMatrix: documentTopicMatrix''
                      , documentWord:
                        DocumentWord
                          (
                            documentWord
                              {
                                  topic = (Just newTopic)
                                , topicIndex = maybeNewTopicIndex
                              }
                          )
                    }
                  )
        _ -> pure returnOnFailure

documentWordTopicProbablities
  ::  forall t
  .   {
          alpha :: Number
        , beta :: Number
        , topicCount :: Number
        , vocabCount :: Number
        , vocabTopicMatrix :: Array (Array Int)
        , documentTopicMatrix :: Array (Array Int)
        , documentWord :: DocumentWord t
      }
  ->  Maybe (Array (Number))
documentWordTopicProbablities
  {
      documentWord: (
      DocumentWord {
          topicIndex: Nothing
      }
    )
  }
  = Nothing
documentWordTopicProbablities
  {
      documentWord: (
      DocumentWord {
          vocabIndex: Nothing
      }
    )
  }
  = Nothing
documentWordTopicProbablities
  {
      alpha: alpha
    , beta: beta
    , topicCount: topicCount
    , vocabCount: vocabCount
    , vocabTopicMatrix: vocabTopicMatrix
    , documentTopicMatrix: documentTopicMatrix
    , documentWord: (
      DocumentWord documentWord@{
          word: word
        , topicIndex: (Just currentTopicIndex)
        , vocabIndex: (Just vocabIndex)
        , documentWordIndex: documentWordIndex
        , documentIndex: documentIndex
      }
    )
  }
  =
  Just (
    map (flip (/) unormalizedProbabilitiesSum) unormalizedProbabilities
  )
  where
    unormalizedProbabilitiesSum :: Number
    unormalizedProbabilitiesSum = sum unormalizedProbabilities
    unormalizedProbabilities :: Array Number
    unormalizedProbabilities =
      foldr (\ topicIndex probabilities ->
        Array.cons
          (
            documentWordTopicProbability
              {
                  alpha: alpha
                , beta: beta
                , topicCount: topicCount
                , vocabCount: vocabCount
                , vocabTopicCount: vocabTopicCount topicIndex
                , totalVocabTopicCount: totalVocabTopicCount topicIndex
                , vocabDocumentTopicCount: vocabDocumentTopicCount topicIndex
                , totalVocabDocumentTopicCount: totalVocabDocumentTopicCount
              }
          )
          probabilities
      ) [] (Array.range 0 ((ceil topicCount) - 1))
    vocabTopicCount :: Int -> Number
    vocabTopicCount =
      (countInRow vocabTopicRow)
    totalVocabTopicCount :: Int -> Number
    totalVocabTopicCount =
      columnSum vocabTopicMatrix
    vocabDocumentTopicCount :: Int -> Number
    vocabDocumentTopicCount =
      (countInRow documentTopicRow)
    totalVocabDocumentTopicCount :: Number
    totalVocabDocumentTopicCount =
      sum documentTopicRow
    vocabTopicRow :: Array Number
    vocabTopicRow =
      matrixRow vocabTopicMatrix vocabIndex
    documentTopicRow :: Array Number
    documentTopicRow =
      matrixRow documentTopicMatrix documentIndex
    countInRow :: Array Number -> Int -> Number
    countInRow row index =
      case (Array.index row index) of
        Nothing -> 0.0
        Just c  -> c
    matrixRow :: Array (Array Int) -> Int -> Array Number
    matrixRow matrix index =
      case (Array.index matrix index) of
        Nothing  -> []
        Just row -> map toNumber row

documentWordTopicProbability
  ::  {
          alpha :: Number
        , beta :: Number
        , topicCount :: Number
        , vocabCount :: Number
        , vocabTopicCount :: Number
        , totalVocabTopicCount :: Number
        , vocabDocumentTopicCount :: Number
        , totalVocabDocumentTopicCount :: Number
      }
  ->  Number
documentWordTopicProbability
  {
      alpha: alpha
    , beta: beta
    , topicCount: topicCount
    , vocabCount: vocabCount
    , vocabTopicCount: vocabTopicCount
    , totalVocabTopicCount: totalVocabTopicCount
    , vocabDocumentTopicCount: vocabDocumentTopicCount
    , totalVocabDocumentTopicCount: totalVocabDocumentTopicCount
  }
  =
  (
      (
          (vocabTopicCount + beta) /
          (totalVocabTopicCount + (vocabCount * beta))
      ) *
      (
          (vocabDocumentTopicCount + alpha) /
          (totalVocabDocumentTopicCount + (topicCount * alpha))
      )
  )

estimateTheta :: Number -> Array (Array Int) -> Array (Array Number)
estimateTheta _     []                  = []
estimateTheta alpha documentTopicMatrix =
  foldr
    (\ (Tuple doc di) docs ->
      Array.cons
        (
          map
            (\ x ->
              case Array.index sums di of
                Nothing -> 0.0
                Just s  ->
                  if s == 0.0
                    then 0.0
                    else (alpha + toNumber x) / s
            )
            doc
        )
        docs
    )
    []
    (zipArrayWithIndices documentTopicMatrix)
  where
    sums :: Array Number
    sums = rowSums $ addToAllRows alpha $ intMatrixToNumberMatrix documentTopicMatrix

estimatePhi :: Number -> Array (Array Int) -> Array (Array Number)
estimatePhi _    []               = []
estimatePhi beta vocabTopicMatrix =
  transpose 0.0 $
    foldr
      (\ vocab vocabs ->
        Array.cons
          (
            map
              (\ (Tuple x t) ->
                case Array.index sums t of
                  Nothing -> 0.0
                  Just s  ->
                    if s == 0.0
                      then 0.0
                      else (beta + toNumber x) / s
              )
              (zipArrayWithIndices vocab)
          )
          vocabs
      )
      []
      vocabTopicMatrix
  where
    sums :: Array Number
    sums =
      rowSums $ transpose 0.0 $ addToAllRows beta $ intMatrixToNumberMatrix vocabTopicMatrix
