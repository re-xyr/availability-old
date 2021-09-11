module Availability.Embed (Embed (..), embed, Unembed (..), withUnembed) where

import           Availability.Impl

data Embed (m' :: * -> *) :: Effect where
  Embed :: m' a -> Embed m' m a

embed :: forall m' m a. Sendable (Embed m') m => m' a -> M m a
embed m = send (Embed m)
{-# INLINE embed #-}

data Unembed (m' :: * -> *) :: Effect where
  WithUnembed :: ((M m a -> m' a) -> m' b) -> Unembed m' m b

withUnembed :: forall m' m a b. Sendable (Unembed m') m => ((M m a -> m' a) -> m' b) -> M m b
withUnembed f = send (WithUnembed f)
{-# INLINE withUnembed #-}
