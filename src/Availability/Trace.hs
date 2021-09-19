module Availability.Trace (Trace (..), trace, TraceByIO (..), TraceByTeller (..)) where

import           Availability
import           Availability.Embed
import           Availability.Writer (Teller, tell)
import           Control.Monad.Trans (MonadIO)

data Trace :: Effect where
  Trace :: String -> Trace m ()

trace :: forall m. Sendable Trace m => String -> M m ()
trace s = send (Trace s)
{-# INLINE trace #-}

newtype TraceByIO m a = TraceByIO (m a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance Interprets '[Embed IO] m => Interpret Trace (TraceByIO m) where
  type InTermsOf _ _ = '[Embed IO]
  {-# INLINE interpret #-}
  interpret (Trace s) = coerceM @m $ embed $ putStrLn s

newtype TraceByTeller tag m a = TraceByTeller (m a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance Interprets '[Teller tag [String]] m => Interpret Trace (TraceByTeller tag m) where
  type InTermsOf _ _ = '[Teller tag [String]]
  {-# INLINE interpret #-}
  interpret (Trace s) = coerceM @m $ tell @tag [s]
