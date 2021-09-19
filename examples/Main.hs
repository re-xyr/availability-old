module Main where

import qualified Teletype
import           Test.Hspec        (describe)
import           Test.Hspec.Runner (defaultConfig, hspecWith)
import qualified TestParity

main :: IO ()
main = hspecWith defaultConfig do
  describe "TestParity" TestParity.spec
  describe "Teletype" Teletype.spec
