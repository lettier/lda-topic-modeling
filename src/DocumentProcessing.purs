{-
  (C) 2018 David Lettier
  lettier.com
  LDA Topic Modeling
-}

module DocumentProcessing where

import Prelude
import Data.Maybe
import Data.Map as Map
import Data.List as List
import Data.Array as Array
import Data.String as String
import Data.Foldable

import ArrayUtils

punctuation :: Array String
punctuation = String.split (String.Pattern "") "`~!@#$%^&*()-=_+[]\\{}|;':\",./<>?“”‘’"

replaceAll :: String -> String -> String -> String
replaceAll p r s = String.replaceAll (String.Pattern p) (String.Replacement r) s

removePunctuation :: String -> String
removePunctuation = (flip (foldl (\ string p -> replaceAll p " " string))) punctuation

removeNullFromStringArray :: Array String -> Array String
removeNullFromStringArray = Array.filter (not String.null)

splitStringOnWhiteSpace :: String -> Array String
splitStringOnWhiteSpace = removeNullFromStringArray <<< (String.split (String.Pattern " "))

cleanAndSplitString :: String -> Array String
cleanAndSplitString = splitStringOnWhiteSpace <<< removePunctuation <<< String.toLower

vocabulary :: Array String -> Array String
vocabulary = Array.sort <<< uniqueArray <<< cleanAndSplitString <<< combineWith " "

generateDocumentVocabMatrix :: Array String -> Array String -> Array (Array Int)
generateDocumentVocabMatrix documents vocabulary =
  map
    (\ x -> countOccurrences x)
    documents
  where
    countOccurrences :: String -> Array Int
    countOccurrences document =
      map
        (\ x -> fromMaybe 0 (Map.lookup x countMap))
        vocabulary
      where
        countMap :: Map.Map String Int
        countMap =
          foldl
            (\ xs x -> Map.alter alter x xs)
            Map.empty
            (cleanAndSplitString document)
          where
            alter :: Maybe Int -> Maybe Int
            alter (Just v) = (Just (v + 1))
            alter Nothing = (Just 1)
