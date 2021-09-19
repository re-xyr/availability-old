module TestParity where

import           Availability
import           Availability.Embed
import           Availability.Lens
import           Availability.Reader
import           Availability.State
import           Control.Monad.Reader    (MonadIO, MonadReader, ReaderT (runReaderT))
import           Data.Function           ((&))
import           Data.IORef              (IORef, newIORef, readIORef)
import           GHC.Generics
import           Test.Hspec              (Spec, it)
import           Test.QuickCheck         (Testable (property))
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

data Ctx = Ctx { foo :: Int, bar :: IORef Bool } deriving (Generic)

newtype App a = App { runApp :: ReaderT Ctx IO a }
  deriving (Functor, Applicative, Monad)
  deriving (MonadIO, MonadReader Ctx)
  deriving (Interpret (Embed IO))
    via ViaMonadIO App
  deriving (Interpret (Getter "foo" Int))
    via FromHas "foo" () Ctx (ViaMonadReader App)
  deriving (Interpret (Putter "bar" Bool))
    via StateByIORef () Bool (FromHas "bar" () Ctx (ViaMonadReader App))

testParity :: (Effs '[Getter "foo" Int, Putter "bar" Bool]) => M App ()
testParity = do
  num <- get @"foo" @Int
  put @"bar" (even num)

spec :: Spec
spec = it "tests parity" do
  property \n -> monadicIO do
    rEven <- run $ newIORef False
    run $ runUnderlying @'[Getter "foo" Int, Putter "bar" Bool] testParity
      & runApp
      & (`runReaderT` Ctx n rEven)
    b <- run $ readIORef rEven
    assert (b == even n)
