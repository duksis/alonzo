module ConfigSpec (main, spec) where

import           Helper

import           System.SetEnv
import           Data.Default

import           Config
import           IRC

main :: IO ()
main = hspec spec

spec :: Spec
spec = withEmptyEnvironment $ do
  describe "readNick" $ do
    it "return default alonzo" $ do
      readNick `shouldReturn` "alonzo"

    context "when environment variable ALONZO_NICK is set" $ do
      it "" $ do
        setEnv "ALONZO_NICK" "foobar"
        readNick `shouldReturn` "foobar"

  describe "readConfig" $ do
    it "return default config" $ do
      readConfig `shouldReturn` def

    context "when environment variable ALONZO_HOST is set" $ do
      it "sets host to specified value" $ do
        setEnv "ALONZO_HOST" "example.com"
        readConfig `shouldReturn` def {host = "example.com"}

    context "when environment variable ALONZO_PORT is set" $ do
      it "sets port to specified value" $ do
        setEnv "ALONZO_PORT" "8080"
        readConfig `shouldReturn` def {port = 8080}

    context "when environment variable ALONZO_PASSWORD is set" $ do
      it "sets password to specified value" $ do
        setEnv "ALONZO_PASSWORD" "secret"
        readConfig `shouldReturn` def {password = Just "secret"}

    context "when environment variable ALONZO_USE_SSL is set to true" $ do
      it "sets ssl to True" $ do
        setEnv "ALONZO_USE_SSL" "true"
        readConfig `shouldReturn` def {ssl = True}

    context "when environment variable ALONZO_VERIFY_SSL is set false" $ do
      it "sets verify to False" $ do
        setEnv "ALONZO_VERIFY_SSL" "false"
        readConfig `shouldReturn` def {verify = False}
