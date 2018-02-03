{-
  (C) 2018 David Lettier
  lettier.com
  LDA Topic Modeling
-}

module UserInterface where

import Prelude
import Data.Tuple
import Data.Maybe
import Data.Array as Array
import Data.String as String
import Data.Int
import Data.Number as Number
import Data.Foldable
import Data.Traversable
import Data.Decimal (toFixed, fromNumber)
import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import DOM
import Unsafe.Coerce
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Random
import Control.Monad.Eff.Console
import Control.Monad.Aff
import Control.Monad.Trans.Class

import ArrayUtils
import DocumentProcessing
import DocumentWord
import DocumentGeneration
import Lda

data Action =
    AdjustTopicCount     String
  | AdjustAlpha          String
  | AdjustBeta           String
  | AdjustIterations     String
  | AddDocument
  | UpdateDocument       Int String
  | RemoveDocument       Int
  | GenerateExample
  | RunLda

type State =
  {
      topicCount :: Int
    , alpha :: Number
    , beta :: Number
    , iterations :: Int
    , documents :: Array String
    , vocabulary :: Array String
    , documentVocabMatrix :: Array (Array Int)
    , documentTopicMatrix :: Array (Array Int)
    , vocabTopicMatrix :: Array (Array Int)
    , performingLda :: Boolean
  }


initialState :: State
initialState =
  {
      topicCount: 3
    , alpha: 1.0 / 3.0
    , beta: 0.01
    , iterations: 100
    , documents: []
    , vocabulary: []
    , documentVocabMatrix: []
    , documentTopicMatrix: []
    , vocabTopicMatrix: []
    , performingLda: false
  }

performAction :: T.PerformAction _ State _ Action
performAction (AdjustTopicCount value) _ _ = do
  let topicCount = fromMaybe initialState.topicCount (fromString value)
  void $
    T.cotransform $
      \ state ->
        state
          {
              topicCount = topicCount
            , vocabTopicMatrix = []
            , documentTopicMatrix = []
          }
performAction (AdjustAlpha value) _ _ =
  void $
    T.cotransform
      \ state ->
        state
          {
              alpha = fromMaybe initialState.alpha (Number.fromString value)
            , vocabTopicMatrix = []
            , documentTopicMatrix = []
          }
performAction (AdjustBeta value) _ _ =
  void $
    T.cotransform $
      \ state ->
        state
          {
              beta = fromMaybe initialState.beta (Number.fromString value)
            , vocabTopicMatrix = []
            , documentTopicMatrix = []
          }
performAction (AdjustIterations value) _ _ =
  void $
    T.cotransform $
      \ state ->
        state { iterations = fromMaybe initialState.iterations (fromString value) }
performAction AddDocument _ { documents } = do
  let documents' = documents <> [""]
  let vocabulary' = vocabulary documents'
  let documentVocabMatrix =
        generateDocumentVocabMatrix documents' vocabulary'
  void $
    T.cotransform $
      \ state ->
        state
          {
              documents = documents'
            , vocabulary = vocabulary'
            , documentVocabMatrix = documentVocabMatrix
            , documentTopicMatrix = []
            , vocabTopicMatrix = []
            , performingLda = false
          }
performAction (UpdateDocument i value) _ { documents } = do
  let documents' =
        foldr
          (\ (Tuple d di) ds ->
            if i == di
              then Array.cons value ds
              else Array.cons d ds
          )
        []
        (zipArrayWithIndices documents)
  let vocabulary' = vocabulary documents'
  let documentVocabMatrix =
        generateDocumentVocabMatrix documents' vocabulary'
  void $
    T.cotransform $
      \ state ->
        state
          {
              documents = documents'
            , vocabulary = vocabulary'
            , documentVocabMatrix = documentVocabMatrix
            , documentTopicMatrix = []
            , vocabTopicMatrix = []
            , performingLda = false
          }
performAction (RemoveDocument i) _ { documents } = do
  let documents' =
        foldr
          (\ (Tuple d di) ds ->
            if i == di
              then ds
              else Array.cons d ds
          )
          []
          (zipArrayWithIndices documents)
  let vocabulary' = vocabulary documents'
  let documentVocabMatrix =
        generateDocumentVocabMatrix documents' vocabulary'
  void $
    T.cotransform $
      \ state ->
        state
          {
              documents = documents'
            , vocabulary = vocabulary'
            , documentVocabMatrix = documentVocabMatrix
            , documentTopicMatrix = []
            , vocabTopicMatrix = []
            , performingLda = false
          }
performAction GenerateExample _ { topicCount, alpha, beta } = do
  let documentCount = 4
  documents <-
    liftEff $
      generateDocuments
        documentCount
        (map (const 45) (intToRange documentCount))
        (generateTopics topicCount)
        ["☀", "😺", "🎩"]
        alpha
        beta
  let vocabulary' = vocabulary documents
  let documentVocabMatrix =
        generateDocumentVocabMatrix documents vocabulary'
  void $
    T.cotransform $
      \ state ->
        state
          {
              documents = documents
            , vocabulary = vocabulary'
            , documentVocabMatrix = documentVocabMatrix
            , vocabTopicMatrix = []
            , documentTopicMatrix = []
          }
performAction
  RunLda
  _
  {
      topicCount
    , alpha
    , beta
    , iterations
    , documents
  }
  = do
  void $
    T.cotransform $
      \ state ->
        state
          {
              documentTopicMatrix = []
            , vocabTopicMatrix = []
            , performingLda = false
          }
  when (documentsAreValid documents) $ do
    let vocabulary' = vocabulary documents
    let topics = generateTopics topicCount
    void $ T.cotransform $ \ state -> state { performingLda = true }
    {
        documentTopicMatrix
      , vocabTopicMatrix
    } <-
      lift $
        performLdaAsyncViaWorker
          {
              alpha: alpha
            , beta: beta
            , topics: topics
            , iterations: iterations
            , documents: documents
            , vocabulary: vocabulary'
          }
    void $ liftEff $ log "performLdaAsyncViaWorker: vocabTopicMatrix"
    void $ liftEff $ logShow vocabTopicMatrix
    void $ liftEff $ log "-----------------------------------"
    void $ liftEff $ log "performLdaAsyncViaWorker: documentTopicMatrix"
    void $ liftEff $ logShow documentTopicMatrix
    void $ liftEff $ log "-----------------------------------"
    void $
        T.cotransform
          \ state ->
            state
              {
                  performingLda = false
                , vocabulary = vocabulary'
                , documentTopicMatrix = documentTopicMatrix
                , vocabTopicMatrix = vocabTopicMatrix
              }

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render

runUi :: forall e. Eff (dom :: DOM, random :: RANDOM, console :: CONSOLE | e) Unit
runUi = T.defaultMain spec initialState unit

makeColums :: Array String -> Array R.ReactElement -> R.ReactElement
makeColums classes = R.div [RP.className (combineWith " " (Array.union ["columns"] classes))]

makeColumns' :: Array R.ReactElement -> R.ReactElement
makeColumns' = R.div [RP.className "columns"]

makeColumn :: Array String -> Array R.ReactElement -> R.ReactElement
makeColumn classes = R.div [RP.className (combineWith " " (Array.union ["column"] classes))]

makeColumn' :: Array R.ReactElement -> R.ReactElement
makeColumn' = R.div [RP.className "column"]

makeNarrowColumn :: Array String -> Array R.ReactElement -> R.ReactElement
makeNarrowColumn classes = makeColumn (["is-narrow"] <> classes)

makeNarrowColumn' :: Array R.ReactElement -> R.ReactElement
makeNarrowColumn' = makeColumn ["is-narrow"]

render :: T.Render State _ Action
render dispatch _ state _ =
  [
      R.div [RP.className "container is-fluid padding-top-10"]
        [
            makeColumns'
              [
                  sidebar dispatch state
                , mainBar dispatch state
              ]
          , makeColumns'
              [
                  makeColumn'
                    [R.p' [R.text "© 2018 David Lettier"]]
              ]
        ]
  ]

sidebar :: (Action -> T.EventHandler) -> State -> R.ReactElement
sidebar dispatch state =
  R.div
    [RP.className "tile is-3 is-vertical is-parent"]
    [
        R.div [RP.className "tile is-child no-flex-grow"]
        [
            R.div
              [RP.className "has-text-centered"]
              [
                  R.a
                    [RP.href "http://www.lettier.com/", RP.title "Lettier.com"]
                    [R.img [RP.className "logo", RP.src "lettier-logo.png"] []]
              ]
          , R.h1 [RP.className "title"] [R.text "LDA Topic Modeling"]
          , R.p'
            [
                R.text
                  (
                    "To start, add two or more documents, adjust the controls to the left, and then press \"Run LDA\". "
                    <>
                    "Or press \"Generate Example\" and then \"Run LDA\" for a quick demonstration."
                  )
            ]
          , R.p
            [RP.className "padding-top-20"]
            [
                R.text
                  "When the calculation is done, you'll see two tables showing the mixture of topics for each word and document."
            ]
          , R.p
            [RP.className "padding-top-20"]
            [
                R.text "Check out the source at "
              , R.a
                [RP.href "http://github.com/lettier/lda-topic-modeling/"]
                [R.text "github.com/lettier"]
              , R.text "."
            ]
        ]
      , R.div
          [RP.className "tile is-child box no-flex-grow"]
          [sidebarSlider dispatch AdjustTopicCount state.topicCount 2 4 1.0 "Topics" state.performingLda]
      , R.div
          [RP.className "tile is-child box no-flex-grow"]
          [sidebarSlider dispatch AdjustAlpha state.alpha 0 2 0.01 "Alpha" state.performingLda]
      , R.div
          [RP.className "tile is-child box no-flex-grow"]
          [sidebarSlider dispatch AdjustBeta state.beta 0 2 0.01 "Beta" state.performingLda]
      , R.div
          [RP.className "tile is-child box no-flex-grow"]
          [sidebarSlider dispatch AdjustIterations state.iterations 1 1500 1.0 "Iterations" state.performingLda]
      , R.div
          [RP.className "tile is-child no-flex-grow"]
          [generateExampleButton dispatch state]
      , R.div
          [RP.className "tile is-child no-flex-grow"]
          [runLdaButton dispatch state]
    ]

sidebarSlider
  ::  forall e
  .   Show e
  =>  (Action -> T.EventHandler)
  ->  (String -> Action)
  ->  e
  ->  Int
  ->  Int
  ->  Number
  ->  String
  ->  Boolean
  ->  R.ReactElement
sidebarSlider
  dispatch
  action
  element
  min
  max
  step
  label
  performingLda
  =
  makeColumns'
    [
        makeNarrowColumn'
          [
              R.div
                [RP.className "sidebar-slider-text-label-counter-container"]
                [
                    R.input
                      [
                          RP.className "input is-rounded has-text-right"
                        , RP._type "text"
                        , RP.value (show element)
                        , RP.disabled true
                        , RP.size 1
                      ]
                      []
                ]
            , R.div [RP.className "has-text-centered bold"] [R.text label]
          ]
      , makeColumn'
          [
              R.input
                [
                    RP.className "slider"
                  , RP._type "range"
                  , RP.min (show min)
                  , RP.max (show max)
                  , RP.step (show step)
                  , RP.value (show element)
                  , RP.disabled performingLda
                  , RP.onChange
                    \ event ->
                      dispatch (action (unsafeCoerce event).target.value)
                ]
                []
          ]
    ]

runLdaButton :: (Action -> T.EventHandler) -> State -> R.ReactElement
runLdaButton dispatch state =
  R.button
    (
      [RP.className "button is-info min-width-100-percent"]
      <>
      (
        if state.performingLda || (not $ documentsAreValid state.documents)
          then [RP.disabled true]
          else [RP.onClick \ _ -> dispatch RunLda]
      )
    )
    [
        R.span
          [RP.className "icon"]
          [
              R.i
                [
                  RP.className
                    (
                      "fi fi-spinner-cog"
                      <>
                      (if state.performingLda then " fi-spin" else "")
                    )
                ]
                []
          ]
      , R.span [RP.className "bold"] [R.text "Run LDA"]
    ]

generateExampleButton :: (Action -> T.EventHandler) -> State -> R.ReactElement
generateExampleButton dispatch state =
  R.button
    (
      [RP.className "button is-warning min-width-100-percent"]
      <>
      (
        if state.performingLda
          then [RP.disabled true]
          else [RP.onClick \ _ -> dispatch GenerateExample]
      )
    )
    [
        R.span
          [RP.className "icon"]
          [
              R.i
                [
                  RP.className "fi fi-spinner-cog"
                ]
                []
          ]
      , R.span [RP.className "bold"] [R.text "Generate Example"]
    ]

mainBar :: (Action -> T.EventHandler) -> State -> R.ReactElement
mainBar dispatch state =
  makeColumn ["is-9"]
    [
        makeColumns'
          [
              documentVocabMatrixTable state
          ]
      , makeColumns'
          [
              topicVocabMatrixTable state
            , documentTopicMatrixTable state
          ]
      , makeColumns'
          [
              makeColumn' $
                (
                  documentInputs dispatch state.documents state.performingLda
                  <>
                  [addDocumentButton dispatch state.performingLda]
                )
          ]
    ]

documentVocabMatrixTable :: State -> R.ReactElement
documentVocabMatrixTable { vocabulary, documentVocabMatrix } =
  matrixTable
    {
        columnLabels: vocabulary
      , rowLabels: map (\ i -> "Document " <> (show i)) (intToRange (Array.length documentVocabMatrix))
      , matrix: documentVocabMatrix
      , cellDisplay: show
      , cellClassName: cellClassName
    }
  where
    cellClassName :: Int -> String
    cellClassName n
      | n == 0 = "background-color-gray"
      | otherwise = ""

topicVocabMatrixTable :: State -> R.ReactElement
topicVocabMatrixTable { performingLda: true } = hourGlass
topicVocabMatrixTable { topicCount, vocabulary, vocabTopicMatrix, beta } =
  matrixTable
    {
        columnLabels: map (\ i -> "Topic " <> (show i)) (generateTopics topicCount)
      , rowLabels: vocabulary
      , matrix: transpose 0.0 $ estimatePhi beta vocabTopicMatrix
      , cellDisplay: toFixed 3 <<< fromNumber
      , cellClassName: ldaMatrixCellClassName
    }

documentTopicMatrixTable :: State -> R.ReactElement
documentTopicMatrixTable { performingLda: true } = hourGlass
documentTopicMatrixTable { topicCount, documents, documentTopicMatrix, alpha } =
  matrixTable
    {
        columnLabels: map (\ i -> "Topic " <> (show i)) (generateTopics topicCount)
      , rowLabels: map (\ i -> "Document " <> (show i)) (arrayToRange documents)
      , matrix: estimateTheta alpha documentTopicMatrix
      , cellDisplay: toFixed 3 <<< fromNumber
      , cellClassName: ldaMatrixCellClassName
    }

ldaMatrixCellClassName :: Number -> String
ldaMatrixCellClassName n
  | n <= 1.0 / 3.0 = "background-color-yellow"
  | n <= 2.0 / 3.0 = "background-color-orange"
  | otherwise      = "background-color-red"

hourGlass :: R.ReactElement
hourGlass =
  makeColumn'
    [
        makeColumns'
          [
              makeColumn
                ["has-text-centered", "hour-glass-column"]
                [R.i [RP.className "fi fi-hourglass fi-spin"] []]
          ]
    ]

matrixTable
  ::  forall a
  .   {
          columnLabels :: Array String
        , rowLabels :: Array String
        , matrix :: Array (Array a)
        , cellDisplay :: a -> String
        , cellClassName :: a -> String
      }
  ->  R.ReactElement
matrixTable { matrix: [] } = makeColumn' [R.text ""]
matrixTable
  {
      columnLabels
    , rowLabels
    , matrix
    , cellDisplay
    , cellClassName
  } =
  makeColumn ["overflow-x-auto"]
    [
        R.table [RP.className "table min-width-100-percent"]
          [
              R.thead' [columnLabelsRow]
            , R.tfoot' [columnLabelsRow]
            , R.tbody' $
                map
                  (\ (Tuple l i) ->
                    R.tr' $
                      [
                          R.th' [R.text l]
                      ]
                      <>
                      (
                        map
                        (\ x ->
                          R.td
                            [RP.className $ cellClassName x]
                            [R.text $ cellDisplay x]
                        )
                        (fromMaybe [] (Array.index matrix i))
                      )
                  )
                  (zipArrayWithIndices rowLabels)
          ]
    ]
  where
    columnLabelsRow :: R.ReactElement
    columnLabelsRow =
      R.tr' $ map (\ l -> R.th' [R.text l]) (Array.cons "" columnLabels)

documentInputs :: (Action -> T.EventHandler) -> Array String -> Boolean -> Array R.ReactElement
documentInputs dispatch documents performingLda =
  map
    (\ (Tuple d i) ->
      makeColumns'
        [
            makeNarrowColumn'
              [R.div [RP.className "document-id"] [R.text ("Document " <> show i)]]
          , makeColumn'
              [
                  R.div'
                    [
                        R.input
                          [
                              RP.className "input document-input"
                            , RP._type "text"
                            , RP.value d
                            , RP.size 30
                            , RP.disabled performingLda
                            , RP.onChange
                              (
                                \ e ->
                                  dispatch
                                    (
                                      UpdateDocument
                                        i
                                        (unsafeCoerce e).target.value
                                    )
                              )
                          ]
                          []
                    ]
              ]
          , makeNarrowColumn'
              [
                R.button
                  [
                      RP.className "button is-danger remove-document-buton"
                    , RP.disabled performingLda
                    , RP.onClick \ _ -> dispatch (RemoveDocument i)
                  ]
                  [
                      R.span [RP.className "icon"] [R.i [RP.className "fi fi-trash"] []]
                    , R.span [RP.className "bold"] [R.text "Remove Document"]
                  ]
              ]
        ]
    )
    (zipArrayWithIndices documents)

addDocumentButton :: (Action -> T.EventHandler) -> Boolean -> R.ReactElement
addDocumentButton dispatch performingLda =
  R.button
    [
        RP.className "button is-primary add-document-button min-width-100-percent"
      , RP.disabled performingLda
      , RP.onClick \ _ -> dispatch AddDocument
    ]
    [
        R.span [RP.className "icon"] [R.i [RP.className "fi fi-plus-a"] []]
      , R.span [RP.className "bold"] [R.text "Add Document"]
    ]

documentsAreValid :: Array String -> Boolean
documentsAreValid documents =
  (not $ hasEmptyDocuments documents) && Array.length documents >= 2

hasEmptyDocuments :: Array String -> Boolean
hasEmptyDocuments [] = true
hasEmptyDocuments documents =
  foldl
    (\ b s -> b || (String.length s <= 0))
    false
    documents

generateTopics :: Int -> Array Int
generateTopics topicCount = map (id) (intToRange topicCount)
