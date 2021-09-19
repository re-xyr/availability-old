module Availability.Fresh (Fresh, fresh, FreshByState (..)) where

import           Availability
import           Availability.State
import           Control.Monad.Trans (MonadIO)

data Fresh :: Effect where
  Fresh :: Fresh m Int

fresh :: forall m. Sendable Fresh m => M m Int
fresh = send Fresh
{-# INLINE fresh #-}

newtype FreshByState tag m a = FreshByState (m a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance Interprets '[Getter tag Int, Putter tag Int] m => Interpret Fresh (FreshByState tag m) where
  type InTermsOf _ _ = '[Getter tag Int, Putter tag Int]
  {-# INLINABLE interpret #-}
  interpret Fresh = coerceM @m do
    x <- get @tag
    put @tag (x + 1)
    pure x
