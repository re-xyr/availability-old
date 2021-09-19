module Availability.Lens (FromHas (..), FromAt (..), FromAs (..)) where

import           Availability
import           Availability.Embed
import           Control.Monad.Trans (MonadIO)

newtype FromHas sel otag r m a = FromHas (m a)
  deriving (Functor, Applicative, Monad, MonadIO)
deriving instance Interpret (Embed IO) m => Interpret (Embed IO) (FromHas sel otag r m)

newtype FromAt otag r m a = FromAt (m a)
  deriving (Functor, Applicative, Monad, MonadIO)
deriving instance Interpret (Embed IO) m => Interpret (Embed IO) (FromAt otag r m)

newtype FromAs sel otag r m a = FromAs (m a)
  deriving (Functor, Applicative, Monad, MonadIO)
deriving instance Interpret (Embed IO) m => Interpret (Embed IO) (FromAs sel otag r m)
