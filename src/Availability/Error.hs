module Availability.Error (Thrower (..), throwError, liftEither, Catcher (..), catchError, runError,
                           makeThrowerByException, makeCatcherByUnembedIO) where

import           Availability.Embed
import           Availability.Impl
import           Control.Exception   (catch, throw)
import           Language.Haskell.TH (Dec, Q, Type)

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

makeThrowerByException :: Q Type -> Q Type -> Q [Dec]
makeThrowerByException typ mnd =
  [d|
  instance Interpret (Thrower $typ) $mnd where
    type InTermsOf _ _ = '[]
    {-# INLINE unsafeSend #-}
    unsafeSend (ThrowError e) = throw e
  |]

makeCatcherByUnembedIO :: Q Type -> Q Type -> Q [Dec]
makeCatcherByUnembedIO typ mnd =
  [d|
  instance Interpret (Catcher $typ) $mnd where
    type InTermsOf _ _ = '[Unembed IO]
    {-# INLINE unsafeSend #-}
    unsafeSend (CatchError m h) = withUnembed \unembed -> catch @($typ) (unembed m) (unembed . h)
  |]
