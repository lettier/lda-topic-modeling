{-
  (C) 2018 David Lettier
  lettier.com
  LDA Topic Modeling
-}

module DocumentWord where

import Prelude
import Data.Foldable
import Data.Tuple
import Data.Maybe
import Data.Array as Array
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.Console

import ArrayUtils
import DocumentProcessing
import Stats

data DocumentWord t =
  DocumentWord
    {
        word :: String
      , topic :: Maybe t
      , topicIndex :: Maybe Int
      , vocabIndex :: Maybe Int
      , documentWordIndex :: Int
      , documentIndex :: Int
    }

showDocumentWord :: forall t. Show t => DocumentWord t -> String
showDocumentWord
  (
    DocumentWord {
        word
      , topic
      , topicIndex
      , vocabIndex
      , documentWordIndex
      , documentIndex
    }
  )
  =
  combine
    [
        "{ word: "
      , word
      , ", \ttopic: "
      , show topic
      , ", \ttopicIndex: "
      , show topicIndex
      , ", \tvocabIndex: "
      , show vocabIndex
      , ", \tdocumentWordIndex: "
      , show documentWordIndex
      , ", \tdocumentIndex: "
      , show documentIndex
      , " }"
    ]

logDocumentWords
  ::  forall e t
  .   Show t
  =>  Array (DocumentWord t)
  ->  Eff (console :: CONSOLE | e) Unit
logDocumentWords = traverse_ (log <<< showDocumentWord)

createDocumentWords
  ::  forall e t
  .   Eq t
  =>  Array t
  ->  Array String
  ->  String
  ->  Int
  ->  Eff (random :: RANDOM | e) (Array (DocumentWord t))
createDocumentWords topics vocab documentString documentIndex = do
  let words = cleanAndSplitString documentString
  Array.foldM
    (\ xs (Tuple word i) -> do
      sample <- uniformSample topics
      pure
        (
          Array.cons
            (
              DocumentWord
                {
                    word: word
                  , topic: sample
                  , topicIndex: topicIndex sample
                  , vocabIndex: Array.elemIndex word vocab
                  , documentWordIndex: i
                  , documentIndex: documentIndex
                }
            )
            xs
        )
    )
    []
    (zipArrayWithIndices words)
  where
    topicIndex :: Maybe t -> Maybe Int
    topicIndex Nothing  = Nothing
    topicIndex (Just a) = Array.elemIndex a topics
