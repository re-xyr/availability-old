module Availability.Listener (module Availability.Putter, Listener (..), listen, pass) where

import           Availability.Impl
import           Availability.Putter

data Listener tag w :: Effect where
  Listen :: M m a -> Listener tag w m (a, w)
  Pass :: M m (a, w -> w) -> Listener tag w m a

listen :: forall tag w m a. (Sendable (Listener tag w) m) => M m a -> M m (a, w)
listen m = send (Listen @m @a @tag m)
{-# INLINE listen #-}

pass :: forall tag w m a. (Sendable (Listener tag w) m) => M m (a, w -> w) -> M m a
pass m = send (Pass @m @a @w @tag m)
{-# INLINE pass #-}
