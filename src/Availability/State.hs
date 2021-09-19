module Availability.State (Getter (..), get, Putter (..), put, PutterKV (..), putKV, DeleterKV (..), delKV,
                           state, modify, makePutterFromLens, makePutterKVFromLens, makeDeleterKVFromLens,
                           makeStateByIORef, makeGetterFromLens, makeGetterKVFromLens, makeEffViaMonadState,
                           makeStateFromLens, makeStateKVFromLens, makeLocallyByState) where

import           Availability
import           Availability.Embed
import           Availability.Reader
import           Control.Monad.Catch (bracket)
import qualified Control.Monad.State as MTL
import           Data.IORef          (IORef, readIORef, writeIORef)
import           Language.Haskell.TH (Dec, Exp, Q, Type)
import           Lens.Micro          ((&), (.~), (?~))
import qualified Lens.Micro          as Lens

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

makePutterFromLens :: Q Type -> Q Type -> Q Type -> Q Type -> Q Exp -> Q Type -> Q [Dec]
makePutterFromLens tag typ otag otyp lens mnd =
  [d|
  instance Interpret (Putter $tag $typ) $mnd where
    type InTermsOf _ _ = '[Getter $otag $otyp, Putter $otag $otyp]
    {-# INLINABLE interpret #-}
    interpret (Put x) = do
      s <- get @($otag) @($otyp)
      put @($otag) @($otyp) (s & $lens .~ x)
  |]

makePutterKVFromLens :: Q Type -> Q Type -> Q Type -> Q Type -> Q Type -> Q Type -> Q [Dec]
makePutterKVFromLens tag ktyp vtyp otag otyp mnd =
  [d|
  instance Interpret (PutterKV $tag $ktyp $vtyp) $mnd where
    type InTermsOf _ _ = '[Getter $otag $otyp, Putter $otag $otyp]
    {-# INLINABLE interpret #-}
    interpret (PutKV k v) = do
      s <- get @($otag) @($otyp)
      put @($otag) @($otyp) (s & Lens.at k ?~ v)
  |]

makeDeleterKVFromLens :: Q Type -> Q Type -> Q Type -> Q Type -> Q Type -> Q Type -> Q [Dec]
makeDeleterKVFromLens tag ktyp vtyp otag otyp mnd =
  [d|
  instance Interpret (DeleterKV $tag $ktyp $vtyp) $mnd where
    type InTermsOf _ _ = '[Getter $otag $otyp, Putter $otag $otyp]
    {-# INLINABLE interpret #-}
    interpret (DelKV k) = do
      s <- get @($otag) @($otyp)
      put @($otag) @($otyp) (s & Lens.at k .~ Nothing)
  |]

makeEffViaMonadState :: Q Type -> Q Type -> Q Type -> Q [Dec]
makeEffViaMonadState tag typ mnd =
  [d|
  instance Interpret (Getter $tag $typ) $mnd where
    type InTermsOf _ _ = '[Underlying]
    {-# INLINE interpret #-}
    interpret Get = underlie MTL.get

  instance Interpret (Putter $tag $typ) $mnd where
    type InTermsOf _ _ = '[Underlying]
    {-# INLINE interpret #-}
    interpret (Put x) = underlie $ MTL.put x
  |]

makeStateByIORef :: Q Type -> Q Type -> Q Type -> Q Type -> Q [Dec]
makeStateByIORef tag typ otag mnd =
  [d|
  instance Interpret (Getter $tag $typ) $mnd where
    type InTermsOf _ _ = '[Getter $otag (IORef $typ), Embed IO]
    {-# INLINABLE interpret #-}
    interpret Get = do
      r <- get @($otag)
      embed $ readIORef r

  instance Interpret (Putter $tag $typ) $mnd where
    type InTermsOf _ _ = '[Getter $otag (IORef $typ), Embed IO]
    {-# INLINABLE interpret #-}
    interpret (Put x) = do
      r <- get @($otag)
      embed $ writeIORef r x
  |]

makeStateFromLens :: Q Type -> Q Type -> Q Type -> Q Type -> Q Exp -> Q Type -> Q [Dec]
makeStateFromLens tag typ otag otyp lens mnd = concat <$> sequence
  [ makeGetterFromLens tag typ otag otyp lens mnd
  , makePutterFromLens tag typ otag otyp lens mnd
  ]

makeStateKVFromLens :: Q Type -> Q Type -> Q Type -> Q Type -> Q Type -> Q Type -> Q [Dec]
makeStateKVFromLens tag k v otag otyp mnd = concat <$> sequence
  [ makeGetterKVFromLens tag k v otag otyp mnd
  , makePutterKVFromLens tag k v otag otyp mnd
  , makeDeleterKVFromLens tag k v otag otyp mnd
  ]

-- This is not thread safe + requires MonadMask.
makeLocallyByState :: Q Type -> Q Type -> Q Type -> Q [Dec]
makeLocallyByState tag typ mnd =
  [d|
  instance Interpret (Locally $tag $typ) $mnd where
    type InTermsOf _ _ = '[Getter $tag $typ, Putter $tag $typ, Underlying]
    {-# INLINABLE interpret #-}
    interpret (Local f m) = do
      s <- get @($tag) @($typ)
      underlie $ bracket
        (runM $ modify @($tag) f)
        (const $ runM $ put @($tag) @($typ) s)
        (const $ runUnderlying @'[Getter $tag $typ] m)
  |]
