module Availability.Trace (Trace (..), trace, makeTraceByIO, makeTraceByWriter) where

import           Availability.Embed
import           Availability.Impl
import           Availability.Writer (Teller, tell)
import           Language.Haskell.TH (Dec, Q, Type)

data Trace :: Effect where
  Trace :: String -> Trace m ()

trace :: forall m. Sendable Trace m => String -> M m ()
trace s = send (Trace s)
{-# INLINE trace #-}

makeTraceByIO :: Q Type -> Q [Dec]
makeTraceByIO mnd =
  [d|
  instance Interpret Trace $mnd where
    type InTermsOf Trace $mnd = '[Embed IO]
    {-# INLINE unsafeSend #-}
    unsafeSend (Trace s) = embed $ putStrLn s
  |]

makeTraceByWriter :: Q Type -> Q Type -> Q [Dec]
makeTraceByWriter tag mnd =
  [d|
  instance Interpret Trace $mnd where
    type InTermsOf Trace $mnd = '[Teller $tag [String]]
    {-# INLINE unsafeSend #-}
    unsafeSend (Trace s) = tell @($tag) [s]
  |]
