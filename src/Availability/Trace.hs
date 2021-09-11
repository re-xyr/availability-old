module Availability.Trace (Trace, trace, makeTraceByIO, makeTraceByWriter) where

import           Availability.Impl
import           Availability.Putter
import           Language.Haskell.TH (Dec, Q, Type)

-- data Trace :: Effect where
--   Trace :: String -> Trace m ()

-- trace :: forall m. Sendable Trace m => String -> M m ()
-- trace s = send (Trace s)
-- {-# INLINE trace #-}

data TraceTag
type Trace = Putter TraceTag String

trace :: forall m. Sendable Trace m => String -> M m ()
trace = put @TraceTag
{-# INLINE trace #-}

makeTraceByIO :: Q Type -> Q [Dec]
makeTraceByIO mnd =
  [d|
  instance Interpret Trace $mnd where
    type InTermsOf Trace $mnd = '[Embed IO]
    {-# INLINE unsafeSend #-}
    unsafeSend (Put s) = embed $ putStrLn s
  |]

makeTraceByWriter :: Q Type -> Q Type -> Q [Dec]
makeTraceByWriter tag mnd =
  [d|
  instance Interpret Trace $mnd where
    type InTermsOf Trace $mnd = '[Putter $tag [String]]
    {-# INLINE unsafeSend #-}
    unsafeSend (Put s) = put @($tag) [s]
  |]
