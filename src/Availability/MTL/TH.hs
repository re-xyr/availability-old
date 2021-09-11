module Availability.MTL.TH where

import           Availability.Embed
import           Availability.Error
import           Availability.Impl
import           Availability.Listener
import           Availability.Locally
import qualified Control.Monad.Catch   as MTL
import qualified Control.Monad.Except  as MTL
import qualified Control.Monad.Reader  as MTL
import qualified Control.Monad.State   as MTL
import qualified Control.Monad.Writer  as MTL
import           Language.Haskell.TH   (Dec, Q, Type)
import           UnliftIO              (MonadUnliftIO (withRunInIO))

makeEffViaMonadReader :: Q Type -> Q Type -> Q Type -> Q [Dec]
makeEffViaMonadReader tag typ mnd =
  [d|
  instance Interpret (Getter $tag $typ) $mnd where
    type InTermsOf (Getter $tag $typ) $mnd = '[Underlying]
    {-# INLINE unsafeSend #-}
    unsafeSend Get = underlie MTL.ask

  instance Interpret (Locally $tag $typ) $mnd where
    type InTermsOf (Locally $tag $typ) $mnd = '[Underlying]
    {-# INLINE unsafeSend #-}
    unsafeSend (Local f m) = underlie $ MTL.local f (runM m)
  |]

makeEffViaMonadState :: Q Type -> Q Type -> Q Type -> Q [Dec]
makeEffViaMonadState tag typ mnd =
  [d|
  instance Interpret (Getter $tag $typ) $mnd where
    type InTermsOf (Getter $tag $typ) $mnd = '[Underlying]
    {-# INLINE unsafeSend #-}
    unsafeSend Get = underlie MTL.get

  instance Interpret (Putter $tag $typ) $mnd where
    type InTermsOf (Putter $tag $typ) $mnd = '[Underlying]
    {-# INLINE unsafeSend #-}
    unsafeSend (Put x) = underlie $ MTL.put x
  |]

makeEffViaMonadIO :: Q Type -> Q [Dec]
makeEffViaMonadIO mnd =
  [d|
  instance Interpret (Embed IO) $mnd where
    type InTermsOf (Embed IO) $mnd = '[Underlying]
    {-# INLINE unsafeSend #-}
    unsafeSend (Embed m) = underlie $ MTL.liftIO m
  |]

makeEffViaMonadWriter :: Q Type -> Q Type -> Q Type -> Q [Dec]
makeEffViaMonadWriter tag typ mnd =
  [d|
  instance Interpret (Putter $tag $typ) $mnd where
    type InTermsOf (Putter $tag $typ) $mnd = '[Underlying]
    {-# INLINE unsafeSend #-}
    unsafeSend (Put x) = underlie $ MTL.tell x

  instance Interpret (Listener $tag $typ) $mnd where
    type InTermsOf (Listener $tag $typ) $mnd = '[Underlying]
    {-# INLINE unsafeSend #-}
    unsafeSend (Listen m) = underlie $ MTL.listen (runM m)
    unsafeSend (Pass m)   = underlie $ MTL.pass (runM m)
  |]

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

makeEffViaMonadUnliftIO :: Q Type -> Q [Dec]
makeEffViaMonadUnliftIO mnd =
  [d|
  instance Interpret (Unembed IO) $mnd where
    type InTermsOf _ _ = '[Underlying]
    {-# INLINE unsafeSend #-}
    unsafeSend (WithUnembed f) = underlie $ withRunInIO \unlift -> f (unlift . runM)
  |]
