module CountLog where

import           Availability
import           Availability.Embed
import           Availability.Fresh
import           Availability.Lens
import           Availability.Reader
import           Availability.Trace
import           Control.Monad.Reader       (MonadReader, ReaderT)
import           Control.Monad.State.Strict (State)
import           Control.Monad.Trans        (MonadIO)
import           Data.Char                  (toUpper)
import           GHC.Generics               (Generic)

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

newtype Log m a = Log { runLog :: ReaderT LogCtx m a }
  deriving (Functor, Applicative, Monad)
  deriving (MonadIO, MonadReader LogCtx)
  deriving (Interpret (Embed IO))
    via ViaMonadIO (Log m)
  deriving (Interpret (Getter "logger" (String -> IO ())))
    via FromHas "logger" () LogCtx (ViaMonadReader (Log m))
  deriving (Interpret Trace)
    via ByLoggerReader (Log m)

-- Fresh effect example program --
doubleCount :: (Sendable Fresh m) => M m Int
doubleCount = fresh >> fresh

-- StateT instance --

type Counter = State Int
