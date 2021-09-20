module Main where

import qualified Teletype
import           Test.Hspec (describe, hspec)
import qualified TestParity

main :: IO ()
main = hspec do
  describe "TestParity" TestParity.spec
  describe "Teletype" Teletype.spec
