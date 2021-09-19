{-# OPTIONS_GHC -Wno-orphans #-}
module Teletype where

import           Availability
import           Availability.Embed
import           Availability.State
import           Availability.Writer
import qualified Control.Monad.State     as MTL
import qualified Control.Monad.Writer    as MTL
import           Data.Function           ((&))
import           System.IO.Silently      (capture_)
import           Test.Common             (withInput)
import           Test.Hspec              (Spec, context, it)
import           Test.QuickCheck         (Testable (property), elements, generate, listOf)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

-- Teletype Effect --

data Teletype :: Effect where
  ReadTTY :: Teletype m String
  WriteTTY :: String -> Teletype m ()

readTTY :: Sendable Teletype m => M m String
readTTY = send ReadTTY

writeTTY :: Sendable Teletype m => String -> M m ()
writeTTY s = send (WriteTTY s)

-- Pure echo program --

type PureProgram = MTL.WriterT [String] (MTL.State [String])

makeEffViaMonadWriter [t|"out"|] [t|[String]|] [t|PureProgram|]
makeEffViaMonadState  [t|"in" |] [t|[String]|] [t|PureProgram|]

instance Interpret Teletype PureProgram where
  type InTermsOf _ _ = '[Getter "in" [String], Putter "in" [String], Teller "out" [String]]
  interpret = \case
    ReadTTY -> get @"in" >>= \case
      []     -> pure ""
      x : xs -> x <$ put @"in" xs
    WriteTTY msg -> tell @"out" [msg]

echoPure :: Eff Teletype => M PureProgram ()
echoPure = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i >> echoPure

runEchoPure :: [String] -> [String]
runEchoPure s = runUnderlying @'[Teletype] echoPure & MTL.execWriterT & (`MTL.evalState` s)

-- Impure echo program --

makeEffViaMonadIO [t|IO|]

instance Interpret Teletype IO where
  type InTermsOf _ _ = '[Embed IO]
  interpret = \case
    ReadTTY      -> embed getLine
    WriteTTY msg -> embed $ putStrLn msg

echoIO :: Eff Teletype => M IO ()
echoIO = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i >> echoIO

main :: IO ()
main = runUnderlying @'[Teletype] echoIO

spec :: Spec
spec = do
  context "echoPure" $
    it "echoes correctly" do
      property \xs -> runEchoPure xs == takeWhile (/= "") xs
  context "echoIO" $
    it "echoes correctly" do
      property $ monadicIO do
        xs <- run $ generate $ listOf $ listOf $ elements ['a'..'z']
        let xs' = filter (not . ('\n' `elem`)) xs ++ [""]
        out <- run $ capture_ $ main `withInput` unlines xs'
        assert $ lines out == takeWhile (/= "") xs'
