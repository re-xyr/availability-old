module Availability.Error (Thrower (..), throwError, liftEither, Catcher (..), catchError, runError,
                           makeEffViaMonadError, makeEffViaMonadThrow, makeEffViaMonadCatch) where

import           Availability.Impl
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
  CatchError :: M m a -> (e -> M m a) -> Catcher e m a

catchError :: forall e m a. Sendable (Catcher e) m => M m a -> (e -> M m a) -> M m a
catchError m h = send (CatchError @_ @_ @_ m h)
{-# INLINE catchError #-}

-- runError is magical, but safe.
runError :: forall e m a. (Interprets '[Catcher e] m, Applicative m) =>
  (Effs '[Thrower e, Catcher e] => M m a) -> M m (Either e a)
runError m = rips @'[Thrower e, Catcher e] $ (Right <$> m) `catchError` \e -> pure (Left e)
{-# INLINE runError #-}

makeEffViaMonadError :: Q Type -> Q Type -> Q [Dec]
makeEffViaMonadError typ mnd =
  [d|
  instance Interpret (Thrower $typ) $mnd where
    type InTermsOf (Thrower $typ) $mnd = '[Underlying]
    {-# INLINE unsafeSend #-}
    unsafeSend (ThrowError e) = underlie $ MTL.throwError e

  instance Interpret (Catcher $typ) $mnd where
    type InTermsOf (Catcher $typ) $mnd = '[Underlying]
    {-# INLINE unsafeSend #-}
    unsafeSend (CatchError m h) = underlie $ MTL.catchError (runM m) (runM . h)
  |]

makeEffViaMonadThrow :: Q Type -> Q Type -> Q [Dec]
makeEffViaMonadThrow typ mnd =
  [d|
  instance Interpret (Thrower $typ) $mnd where
    type InTermsOf (Thrower $typ) $mnd = '[Underlying]
    {-# INLINE unsafeSend #-}
    unsafeSend (ThrowError e) = underlie $ MTL.throwM e
  |]

makeEffViaMonadCatch :: Q Type -> Q Type -> Q [Dec]
makeEffViaMonadCatch typ mnd =
  [d|
  instance Interpret (Catcher $typ) $mnd where
    type InTermsOf (Catcher $typ) $mnd = '[Underlying]
    {-# INLINE unsafeSend #-}
    unsafeSend (CatchError m h) = underlie $ MTL.catch (runM m) (runM . h)
  |]