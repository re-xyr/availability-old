module Teletype where

import           Availability
import           Availability.Embed
import           Availability.State
import           Availability.Writer
import qualified Control.Monad.State     as MTL
import           Control.Monad.Trans     (MonadIO)
import qualified Control.Monad.Writer    as MTL
import           Data.Function           ((&))
import           System.IO.Silently      (capture_)
import           Test.Common             (withInput)
import           Test.Hspec              (Spec, context, it)
import           Test.QuickCheck         (Testable (property), elements, generate, listOf)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)
import           UnliftIO.Concurrent     (threadDelay)

-- Teletype Effect --

data Teletype :: Effect where
  ReadTTY :: Teletype m String
  WriteTTY :: String -> Teletype m ()

readTTY :: Sendable Teletype m => M m String
readTTY = send ReadTTY

writeTTY :: Sendable Teletype m => String -> M m ()
writeTTY s = send (WriteTTY s)

-- Pure echo program --

newtype PureProgram a = PureProgram { runPureProgram :: MTL.WriterT [String] (MTL.State [String]) a }
  deriving (Functor, Applicative, Monad)
  deriving (MTL.MonadWriter [String], MTL.MonadState [String])
  deriving (Interpret (Teller "out" [String]))
    via ViaMonadWriter PureProgram
  deriving (Interpret (Getter "in" [String]), Interpret (Putter "in" [String]))
    via ViaMonadState PureProgram

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
runEchoPure s = runM @'[Teletype] echoPure & runPureProgram & MTL.execWriterT & (`MTL.evalState` s)

-- Impure echo program --

newtype ImpureProgram a = ImpureProgram { runImpureProgram :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving (Interpret (Embed IO))
    via ViaMonadIO ImpureProgram

instance Interpret Teletype ImpureProgram where
  type InTermsOf _ _ = '[Embed IO]
  interpret = \case
    ReadTTY      -> embed getLine
    WriteTTY msg -> embed $ putStrLn msg

echoIO :: Eff Teletype => M ImpureProgram ()
echoIO = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i >> echoIO

main :: IO ()
main = runM @'[Teletype] echoIO & runImpureProgram

spec :: Spec
spec = do
  context "echoPure" $
    it "echoes correctly" do
      property \xs -> runEchoPure xs == takeWhile (/= "") xs
  context "echoIO" do
    it "echoes correctly" do
      property $ monadicIO do
        xs <- run $ generate $ listOf $ listOf $ elements ['a'..'z']
        let xs' = filter (not . ('\n' `elem`)) xs ++ [""]
        run $ threadDelay 10
        out <- run $ capture_ $ main `withInput` unlines xs'
        run $ print $ takeWhile (/= "") xs'
        run $ print $ lines out
        assert $ lines out == takeWhile (/= "") xs'
