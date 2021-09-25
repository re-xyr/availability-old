module Main where

import qualified CountLog
import qualified Error
import qualified Teletype
import           Test.Hspec (describe, hspec)
import qualified TestParity
import qualified WordCount

main :: IO ()
main = hspec do
  describe "TestParity" TestParity.spec
  describe "Teletype" Teletype.spec
  describe "WordCount" WordCount.spec
  describe "CountLog" CountLog.spec
  describe "Error" Error.spec
