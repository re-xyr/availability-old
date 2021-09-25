-- The original author of this file is EURL Tweag (https://github.com/tweag/capability/blob/master/examples/Test/Common.hs).
-- The use is under the premission of the BSD-3-Clause License (https://github.com/tweag/capability/blob/master/LICENSE.md).
module Test.Common (withInput, shouldPrint, shouldNotPrint, assertPrints, assertDoesn'tPrint) where

import           Control.Exception       (bracket)
import           GHC.IO.Handle           (hDuplicate, hDuplicateTo)
import           System.IO
import           System.IO.Silently      (capture_)
import           System.IO.Temp          (withSystemTempFile)
import           Test.Hspec              (shouldBe, shouldNotBe)
import           Test.QuickCheck.Monadic (PropertyM, assert, run)

-- | Execute the given action with @stdin@ redirected to read the given input.
withInput :: IO a -> String -> IO a
withInput action input = withSystemTempFile "availability-mock-input" \tmpFile tmpOut -> do
  -- write input to temp-file
  hPutStr tmpOut input
  hClose tmpOut
  -- read stdin from temp-file
  buffering <- hGetBuffering stdin
  withFile tmpFile ReadMode \tmpIn -> do
    let
      redirect = do
        old <- hDuplicate stdin
        hDuplicateTo tmpIn stdin
        pure old
      restore old = do
        hDuplicateTo old stdin
        hSetBuffering stdin buffering
        hClose old
    bracket redirect restore (const action)

shouldPrint :: IO a -> String -> IO ()
shouldPrint action expected = do
  actual <- capture_ action
  actual `shouldBe` expected

shouldNotPrint :: IO a -> String -> IO ()
shouldNotPrint action expected = do
  actual <- capture_ action
  actual `shouldNotBe` expected

assertPrints :: IO a -> String -> PropertyM IO ()
assertPrints action expected = do
  actual <- run $ capture_ action
  assert (actual == expected)

assertDoesn'tPrint :: IO a -> String -> PropertyM IO ()
assertDoesn'tPrint action expected = do
  actual <- run $ capture_ action
  assert (actual /= expected)
