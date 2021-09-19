module TestParity where

import           Availability
import           Availability.Embed
import           Availability.Reader
import           Availability.State
import           Control.Monad.Reader    (ReaderT (runReaderT))
import           Data.Function           ((&))
import           Data.IORef              (IORef, newIORef, readIORef)
import           Lens.Micro.TH           (makeLenses)
import           Test.Hspec              (Spec, it)
import           Test.QuickCheck         (Testable (property))
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

data Ctx = Ctx { _foo :: Int, _bar :: IORef Bool }
makeLenses ''Ctx

type App = ReaderT Ctx IO
makeEffViaMonadIO                                                                  [t|App|]
makeEffViaMonadReader [t|"ctx"   |] [t|Ctx       |]                                [t|App|]
makeReaderFromLens    [t|"foo"   |] [t|Int       |] [t|"ctx"   |] [t|Ctx|] [|foo|] [t|App|]
makeReaderFromLens    [t|"barRef"|] [t|IORef Bool|] [t|"ctx"   |] [t|Ctx|] [|bar|] [t|App|]
makeStateByIORef      [t|"bar"   |] [t|Bool      |] [t|"barRef"|]                  [t|App|]

testParity :: (Effs '[Getter "foo" Int, Putter "bar" Bool]) => M App ()
testParity = do
  num <- get @"foo" @Int
  put @"bar" (even num)

spec :: Spec
spec = it "tests parity" do
  property \n -> monadicIO do
    rEven <- run $ newIORef False
    run $ runUnderlying @'[Getter "foo" Int, Putter "bar" Bool] testParity
      & (`runReaderT` Ctx n rEven)
    b <- run $ readIORef rEven
    assert (b == even n)
