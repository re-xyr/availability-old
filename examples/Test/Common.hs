-- The original author of this file is EURL Tweag (https://github.com/tweag/capability/blob/master/examples/Test/Common.hs).
-- The use is under the premission of the BSD-3-Clause License (https://github.com/tweag/capability/blob/master/LICENSE.md).
module Test.Common (withInput) where

import           GHC.IO.Handle      (hDuplicate, hDuplicateTo)
import           System.IO
import           UnliftIO.Exception (bracket)
import           UnliftIO.Temporary (withSystemTempFile)

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
