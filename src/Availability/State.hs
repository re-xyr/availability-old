module Availability.State (Getter (..), get, Putter (..), put, PutterKV (..), putKV, DeleterKV (..), delKV, state,
                           modify, modify', ViaMonadState (..), StateByIORef (..), LocallyByState (..)) where

import           Availability
import           Availability.Embed
import           Availability.Lens
import           Availability.Reader
import           Control.Lens          (At (at), Index, IxValue, (&), (.~), (?~))
import           Control.Monad.Catch   (bracket)
import qualified Control.Monad.Catch   as MTL
import qualified Control.Monad.State   as MTL
import           Control.Monad.Trans   (MonadIO (liftIO))
import           Data.Generics.Product (HasAny (the))
import           Data.IORef            (IORef, readIORef, writeIORef)

data Putter tag s :: Effect where
  Put :: s -> Putter tag s m ()

put :: forall tag s m. Sendable (Putter tag s) m => s -> M m ()
put x = send (Put @_ @tag x)
{-# INLINE put #-}

data PutterKV tag k v :: Effect where
  PutKV :: k -> v -> PutterKV tag k v m ()

putKV :: forall tag k v m. Sendable (PutterKV tag k v) m => k -> v -> M m ()
putKV k v = send (PutKV @_ @_ @tag k v)
{-# INLINE putKV #-}

data DeleterKV tag k v :: Effect where
  DelKV :: k -> DeleterKV tag k v m ()

delKV :: forall tag k v m. Sendable (DeleterKV tag k v) m => k -> M m ()
delKV k = send (DelKV @_ @tag @v k)
{-# INLINE delKV #-}

state :: forall tag s m a. (Sendable (Getter tag s) m, Sendable (Putter tag s) m, Monad m) => (s -> (a, s)) -> M m a
state f = do
  x <- get @tag
  let (r, x') = f x
  put @tag x'
  pure r
{-# INLINABLE state #-}

modify :: forall tag s m. (Sendable (Getter tag s) m, Sendable (Putter tag s) m, Monad m) => (s -> s) -> M m ()
modify f = do
  x <- get @tag
  put @tag (f x)
{-# INLINABLE modify #-}

modify' :: forall tag s m. (Sendable (Getter tag s) m, Sendable (Putter tag s) m, Monad m) => (s -> s) -> M m ()
modify' f = do
  x <- get @tag
  put @tag $! f x
{-# INLINABLE modify' #-}

instance (Interprets '[Getter otag r, Putter otag r] m, HasAny sel r r s s) =>
  Interpret (Putter tag s) (FromHas sel otag r m) where
  type InTermsOf _ _ = '[Getter otag r, Putter otag r]
  {-# INLINABLE interpret #-}
  interpret (Put x) = coerceM @m $ do
    r <- get @otag @r
    put @otag @r (r & the @sel .~ x)

instance (Interprets '[Getter otag r, Putter otag r] m, At r, k ~ Index r, v ~ IxValue r) =>
  Interpret (PutterKV tag k v) (FromAt otag r m) where
  type InTermsOf _ _ = '[Getter otag r, Putter otag r]
  {-# INLINABLE interpret #-}
  interpret (PutKV k v) = coerceM @m do
    r <- get @otag @r
    put @otag @r (r & at k ?~ v)

instance (Interprets '[Getter otag r, Putter otag r] m, At r, k ~ Index r, v ~ IxValue r) =>
  Interpret (DeleterKV tag k v) (FromAt otag r m) where
  type InTermsOf _ _ = '[Getter otag r, Putter otag r]
  {-# INLINABLE interpret #-}
  interpret (DelKV k) = coerceM @m do
    r <- get @otag @r
    put @otag @r (r & at k .~ Nothing)

newtype ViaMonadState m a = ViaMonadState (m a)
  deriving (Functor, Applicative, Monad, MonadIO, MTL.MonadState s)
deriving instance Interpret (Embed IO) m => Interpret (Embed IO) (ViaMonadState m)

instance MTL.MonadState s m => Interpret (Getter tag s) (ViaMonadState m) where
  type InTermsOf _ _ = '[Underlying]
  {-# INLINE interpret #-}
  interpret Get = underlie MTL.get

instance MTL.MonadState s m => Interpret (Putter tag s) (ViaMonadState m) where
  type InTermsOf _ _ = '[Underlying]
  {-# INLINE interpret #-}
  interpret (Put x) = underlie $ MTL.put x

newtype StateByIORef otag s m a = StateByIORef (m a)
  deriving (Functor, Applicative, Monad, MonadIO)
deriving instance Interpret (Embed IO) m => Interpret (Embed IO) (StateByIORef otag s m)

instance Interprets '[Getter otag (IORef s), Embed IO] m => Interpret (Getter tag s) (StateByIORef otag s m) where
  type InTermsOf _ _ = '[Getter otag (IORef s), Embed IO]
  {-# INLINABLE interpret #-}
  interpret Get = coerceM @m $ do
    r <- get @otag
    embed $ readIORef r

newtype StateByIORef' otag s m a = StateByIORef' (m a)
  deriving (Functor, Applicative, Monad, MonadIO)
deriving instance Interpret (Embed IO) m => Interpret (Embed IO) (StateByIORef' otag s m)

instance (Interprets '[Getter otag (IORef s)] m, MonadIO m) => Interpret (Getter tag s) (StateByIORef' otag s m) where
  type InTermsOf _ _ = '[Getter otag (IORef s), Underlying]
  {-# INLINABLE interpret #-}
  interpret Get = coerceM @m $ do
    r <- get @otag
    underlie $ liftIO $ readIORef r

instance Interprets '[Getter otag (IORef s), Embed IO] m => Interpret (Putter tag s) (StateByIORef otag s m) where
  type InTermsOf _ _ = '[Getter otag (IORef s), Embed IO]
  {-# INLINABLE interpret #-}
  interpret (Put x) = coerceM @m $ do
    r <- get @otag
    embed $ writeIORef r x

newtype LocallyByState tag s m a = LocallyByState (m a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- This is not safe + requires MonadMask. You really won't want to do this.
instance (MTL.MonadMask m, Interprets '[Getter tag s, Putter tag s, Underlying] m) =>
  Interpret (Locally tag s) (LocallyByState tag s m) where
  type InTermsOf _ _ = '[Getter tag s, Putter tag s, Underlying]
  {-# INLINABLE interpret #-}
  interpret (Local f m) = coerceM @m do
    s <- get @tag @s
    underlie $ bracket
      (runM' $ modify @tag f)
      (const $ runM' $ put @tag @s s)
      (const $ runM @'[Getter tag s] $ coerceM' @m m)
