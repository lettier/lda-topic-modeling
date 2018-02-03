{-
  (C) 2018 David Lettier
  lettier.com
  LDA Topic Modeling
-}

module Main where

import Prelude
import Data.Foldable
import DOM
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.Console

import UserInterface

main
  ::  forall e
  .   Eff (dom :: DOM, random :: RANDOM, console :: CONSOLE | e) Unit
main = do
  runUi
  pure unit
