module Availability.Error (Thrower (..), throwError, liftEither, Catcher (..), catchError, catchJust, tryError,
                           ViaMonadError (..), ViaMonadThrow (..), ViaMonadCatch (..)) where

import           Availability
import           Availability.Lens
import           Control.Exception    (Exception)
import           Control.Lens         ((#), (^?))
import qualified Control.Monad.Catch  as MTL
import qualified Control.Monad.Except as MTL
import           Control.Monad.Trans  (MonadIO)
import           Data.Generics.Sum    (AsAny (_As))

data Thrower e :: Effect where
  ThrowError :: e -> Thrower e m a

throwError :: forall e m a. Sendable (Thrower e) m => e -> M m a
throwError e = send (ThrowError e)
{-# INLINE throwError #-}

liftEither :: forall e m a. (Sendable (Thrower e) m, Applicative m) => Either e a -> M m a
liftEither = either throwError pure
{-# INLINE liftEither #-}

data Catcher e :: Effect where
  CatchError :: (Eff (Thrower e) => M m a) -> (e -> M m a) -> Catcher e m a

newtype AvailabilityException e = AvailabilityException { runException :: e }
  deriving (Show, Eq, Exception)

catchError :: forall e m a. Sendable (Catcher e) m => (Eff (Thrower e) => M m a) -> (e -> M m a) -> M m a
catchError m h = send (CatchError @_ @_ @_ m h)
{-# INLINE catchError #-}

catchJust :: forall e m b a. (Sendable (Thrower e) m, Sendable (Catcher e) m) =>
  (e -> Maybe b) -> M m a -> (b -> M m a) -> M m a
catchJust f m h = m `catchError` \e -> case f e of
  Nothing -> throwError e
  Just b  -> h b
{-# INLINE catchJust #-}

tryError :: forall e m a. Sendable (Catcher e) m => (Eff (Thrower e) => M m a) -> M m (Either e a)
tryError m = (Right <$> m) `catchError` \e -> pure $ Left e
{-# INLINE tryError #-}

newtype ViaMonadError m a = ViaMonadError (m a)
  deriving (Functor, Applicative, Monad, MonadIO, MTL.MonadError e)

instance MTL.MonadError e m => Interpret (Thrower e) (ViaMonadError m) where
  type InTermsOf _ _ = '[Underlying]
  {-# INLINE interpret #-}
  interpret (ThrowError e) = underlie $ MTL.throwError e

instance MTL.MonadError e m => Interpret (Catcher e) (ViaMonadError m) where
  type InTermsOf _ _ = '[Underlying]
  {-# INLINE interpret #-}
  interpret (CatchError m h) = underlie $ MTL.catchError (runUnderlying @'[Thrower e] m) (runM . h)

newtype ViaMonadThrow m a = ViaMonadThrow (m a)
  deriving (Functor, Applicative, Monad, MonadIO, MTL.MonadThrow)

instance (Exception e, MTL.MonadThrow m) => Interpret (Thrower e) (ViaMonadThrow m) where
  type InTermsOf _ _ = '[Underlying]
  {-# INLINE interpret #-}
  interpret (ThrowError e) = underlie $ MTL.throwM $ AvailabilityException e

newtype ViaMonadCatch m a = ViaMonadCatch (m a)
  deriving (Functor, Applicative, Monad, MonadIO, MTL.MonadThrow, MTL.MonadCatch)

instance (Exception e, MTL.MonadCatch m) => Interpret (Catcher e) (ViaMonadCatch m) where
  type InTermsOf _ _ = '[Underlying]
  {-# INLINE interpret #-}
  interpret (CatchError m h) = underlie $ MTL.catch (runUnderlying @'[Thrower e] m) (runM . h . runException)

instance (Interprets '[Thrower e] m, AsAny sel d e) => Interpret (Thrower d) (FromAs sel otag e m) where
  type InTermsOf _ _ = '[Thrower e]
  {-# INLINE interpret #-}
  interpret (ThrowError d) = coerceM @m $ throwError @e (_As @sel # d)

instance (Interprets '[Catcher e, Thrower e] m, AsAny sel d e) => Interpret (Catcher d) (FromAs sel otag e m) where
  type InTermsOf _ _ = '[Catcher e, Thrower e]
  {-# INLINE interpret #-}
  interpret (CatchError m h) = coerceM @m $
    (coerceM' @m $ derive @(Thrower d) m) `catchError` \(e :: e) ->
      case e ^? _As @sel of
        Nothing -> throwError e
        Just d  -> coerceM' @m $ h d
