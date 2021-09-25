module CountLog where

import           Availability
import           Availability.Embed
import           Availability.Fresh
import           Availability.Lens
import           Availability.Reader
import           Availability.State
import           Availability.Trace
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Reader       (MonadReader, ReaderT (runReaderT))
import           Control.Monad.State.Strict (MonadState, State, runState)
import           Data.Char                  (toUpper)
import           Data.Function              ((&))
import           Data.IORef                 (IORef, newIORef)
import           GHC.Generics               (Generic)
import           Test.Common                (assertPrints, shouldPrint)
import           Test.Hspec                 (Spec, context, describe, it, shouldBe, shouldReturn)
import           Test.QuickCheck            (property)
import           Test.QuickCheck.Monadic    (monadicIO)

-- Trace effect implementation --

newtype ByLoggerReader m a = ByLoggerReader (m a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance Interprets '[Getter "logger" (String -> IO ()), Embed IO] m => Interpret Trace (ByLoggerReader m) where
  type InTermsOf _ _ = '[Getter "logger" (String -> IO ()), Embed IO]
  {-# INLINABLE interpret #-}
  interpret (Trace s) = coerceM @m $ get @"logger" >>= embed @IO . ($ s)

-- Example program --

logNum :: Sendable Trace m => Int -> M m ()
logNum = trace . ("num: " ++) . show
{-# INLINE logNum #-}

-- ReaderT instance --

newtype LogCtx = LogCtx { logger :: String -> IO () } deriving (Generic)

regularLogger :: LogCtx
regularLogger = LogCtx { logger = putStrLn }

loudLogger :: LogCtx
loudLogger = LogCtx { logger = putStrLn . map toUpper }

newtype Log m a = Log (ReaderT LogCtx m a)
  deriving (Functor, Applicative, Monad)
  deriving (MonadIO, MonadReader LogCtx)
  deriving (Interpret (Embed IO)) via ViaMonadIO (Log m)
  deriving (Interpret Trace) via ByLoggerReader (FromHas "logger" () LogCtx (ViaMonadReader (Log m)))

runLog :: LogCtx -> Log m a -> m a
runLog ctx (Log m) = runReaderT m ctx

-- Fresh effect example program --
doubleCount :: (Sendable Fresh m) => M m Int
doubleCount = fresh >> fresh
{-# INLINE doubleCount #-}

-- StateT instance --

newtype Counter a = Counter (State Int a)
  deriving (Functor, Applicative, Monad, MonadState Int)
  deriving (Interpret Fresh) via FreshByState () (ViaMonadState Counter)

runCounter :: Counter a -> (a, Int)
runCounter (Counter m) = runState m 0

-- ReaderT IORef instance --

newtype Counter' m a = Counter' (ReaderT (IORef Int) m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (IORef Int))
  deriving (Interpret (Embed IO)) via ViaMonadIO (Counter' m)
  deriving (Interpret Fresh) via FreshByState () (StateByIORef () (ViaMonadReader (Counter' m)))

runCounter' :: MonadIO m => Counter' m a -> m a
runCounter' (Counter' m) = runReaderT m =<< liftIO (newIORef 0)

-- Mixed program --

data CountLogCtx = CountLogCtx
  { countCtx :: IORef Int
  , logCtx   :: LogCtx
  } deriving Generic

newtype CountLog m a = CountLog (ReaderT CountLogCtx m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader CountLogCtx)
  deriving (Interpret (Embed IO)) via ViaMonadIO (CountLog m)
  deriving (Interpret Fresh) via
    FreshByState () (StateByIORef () (FromHas "countCtx" () CountLogCtx (ViaMonadReader (CountLog m))))
  deriving (Interpret Trace) via
    ByLoggerReader (FromHas "logger" "logCtx" LogCtx (FromHas "logCtx" () CountLogCtx (ViaMonadReader (CountLog m))))

mixed :: Effs '[Trace, Fresh] => M (CountLog IO) ()
mixed = do
  doubleCount >>= logNum
  doubleCount >>= logNum

runCountLog :: MonadIO m => CountLog m b -> m b
runCountLog (CountLog m) = do
  ref <- liftIO $ newIORef 0
  runReaderT m CountLogCtx
    { countCtx = ref
    , logCtx = regularLogger
    }

-- Test cases --

spec :: Spec
spec = do
  describe "Log" do
    context "regularLogger" $
      it "evaluates logNum" $
        property \(n :: Int) -> monadicIO $
          runLog regularLogger (runM @'[Trace] $ logNum n) `assertPrints` ("num: " <> show n <> "\n")
    context "loudLogger" $
      it "evaluates logNum" $
        property \(n :: Int) -> monadicIO $
          runLog loudLogger (runM @'[Trace] $ logNum n) `assertPrints` ("NUM: " <> show n <> "\n")
  describe "Counter" $
    it "evaluates doubleCount" $
      (runM @'[Fresh] doubleCount & runCounter) `shouldBe` (1, 2)
  describe "Counter'" $
    it "evaluates doubleCount" $
      (runM @'[Fresh] doubleCount & runCounter') `shouldReturn` 1
  describe "CountLog" do
    it "evaluates logNum" $
      property \(n :: Int) -> monadicIO $
        runCountLog (runM @'[Trace]  $ logNum n) `assertPrints` ("num: " <> show n <> "\n")
    it "evaluates doubleCount" $
      (runM @'[Fresh] doubleCount & runCountLog) `shouldReturn` 1
    it "evaluates mixed" $
      runCountLog (runM @'[Fresh, Trace] mixed) `shouldPrint` "num: 1\nnum: 3\n"
