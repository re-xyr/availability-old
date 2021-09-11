module Availability.Locally (module Availability.Getter, Locally (..), local, runReader) where

import           Availability.Getter
import           Availability.Impl

data Locally tag s :: Effect where
  Local :: (s -> s) -> M m a -> Locally tag s m a

local :: forall tag s m a. Sendable (Locally tag s) m => (s -> s) -> M m a -> M m a
local f m = send (Local @_ @_ @_ @tag f m)
{-# INLINE local #-}

-- runReader is magical, but safe.
runReader :: forall tag s m a. Sendable (Locally tag s) m =>
  s -> (Effs '[Getter tag s, Locally tag s] => M m a) -> M m a
runReader s m = rips @'[Getter tag s, Locally tag s] $ local @tag (const s) m
{-# INLINE runReader #-}
