module Helper (
  module Test.Hspec
, module Test.QuickCheck
, withEmptyEnvironment
) where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Exception
import           System.Posix.Env

withEmptyEnvironment :: Spec -> Spec
withEmptyEnvironment = around $ \e -> do
  env <- getEnvironment
  bracket_ (setEnvironment []) (setEnvironment env) e
