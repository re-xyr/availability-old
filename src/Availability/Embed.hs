module Availability.Embed (Embed (..), embed, Unembed (..), withUnembed, makeEffViaMonadIO,
                           makeEffViaMonadUnliftIO) where

import           Availability
import           Language.Haskell.TH
import qualified UnliftIO            as MTL

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

makeEffViaMonadIO :: Q Type -> Q [Dec]
makeEffViaMonadIO mnd =
  [d|
  instance Interpret (Embed IO) $mnd where
    type InTermsOf _ _ = '[Underlying]
    {-# INLINE interpret #-}
    interpret (Embed m) = underlie $ MTL.liftIO m
  |]

makeEffViaMonadUnliftIO :: Q Type -> Q [Dec]
makeEffViaMonadUnliftIO mnd =
  [d|
  instance Interpret (Unembed IO) $mnd where
    type InTermsOf _ _ = '[Underlying]
    {-# INLINE interpret #-}
    interpret (WithUnembed f) = underlie $ MTL.withRunInIO \unlift -> f (unlift . runM)
  |]
