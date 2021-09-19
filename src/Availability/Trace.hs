module Availability.Trace (Trace (..), trace, makeTraceByIO, makeTraceByWriter) where

import           Availability
import           Availability.Embed
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
    type InTermsOf _ _ = '[Embed IO]
    {-# INLINE interpret #-}
    interpret (Trace s) = embed $ putStrLn s
  |]

makeTraceByWriter :: Q Type -> Q Type -> Q [Dec]
makeTraceByWriter tag mnd =
  [d|
  instance Interpret Trace $mnd where
    type InTermsOf _ _ = '[Teller $tag [String]]
    {-# INLINE interpret #-}
    interpret (Trace s) = tell @($tag) [s]
  |]
