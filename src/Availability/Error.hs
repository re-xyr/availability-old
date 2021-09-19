module Availability.Error (Thrower (..), throwError, liftEither, Catcher (..), catchError, tryError,
                           makeEffViaMonadError, makeEffViaMonadThrow, makeEffViaMonadCatch) where

import           Availability
import           Control.Exception    (Exception)
import qualified Control.Monad.Catch  as MTL
import qualified Control.Monad.Except as MTL
import           Language.Haskell.TH  (Dec, Q, Type)

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

tryError :: forall e m a. Sendable (Catcher e) m => (Eff (Thrower e) => M m a) -> M m (Either e a)
tryError m = (Right <$> m) `catchError` \e -> pure $ Left e
{-# INLINE tryError #-}

makeEffViaMonadError :: Q Type -> Q Type -> Q [Dec]
makeEffViaMonadError typ mnd =
  [d|
  instance Interpret (Thrower $typ) $mnd where
    type InTermsOf _ _ = '[Underlying]
    {-# INLINE interpret #-}
    interpret (ThrowError e) = underlie $ MTL.throwError e

  instance Interpret (Catcher $typ) $mnd where
    type InTermsOf _ _ = '[Underlying]
    {-# INLINE interpret #-}
    interpret (CatchError m h) = underlie $ MTL.catchError (runUnderlying @'[Thrower $typ] m) (runM . h)
  |]

makeEffViaMonadThrow :: Q Type -> Q [Dec]
makeEffViaMonadThrow mnd =
  [d|
  instance Exception e => Interpret (Thrower e) $mnd where
    type InTermsOf _ _ = '[Underlying]
    {-# INLINE interpret #-}
    interpret (ThrowError e) = underlie $ MTL.throwM $ AvailabilityException e
  |]

makeEffViaMonadCatch :: Q Type -> Q [Dec]
makeEffViaMonadCatch mnd =
  [d|
  instance Exception e => Interpret (Catcher e) $mnd where
    type InTermsOf _ _ = '[Underlying]
    {-# INLINE interpret #-}
    interpret (CatchError m h) = underlie $ MTL.catch (runUnderlying @'[Thrower e] m) (runM . h . runException)
  |]
