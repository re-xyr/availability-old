module Availability.Embed (Embed (..), embed, Unembed (..), withUnembed, ViaMonadIO (..), ViaMonadUnliftIO (..)) where

import           Availability
import           Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO (withRunInIO))

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

newtype ViaMonadIO m a = ViaMonadIO (m a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadIO m => Interpret (Embed IO) (ViaMonadIO m) where
  type InTermsOf _ _ = '[Underlying]
  {-# INLINE interpret #-}
  interpret (Embed m) = underlie $ liftIO m

newtype ViaMonadUnliftIO m a = ViaMonadUnliftIO (m a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadUnliftIO m => Interpret (Unembed IO) (ViaMonadUnliftIO m) where
  type InTermsOf _ _ = '[Underlying]
  {-# INLINE interpret #-}
  interpret (WithUnembed f) = coerceM @m $ underlie $ withRunInIO \unlift -> f (unlift . runM' . coerceM' @m)
