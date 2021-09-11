module Availability.State (module Availability.Getter, module Availability.Putter, state, modify, makeStateByIORef,
                           makeStateFromOptics, makeStateKVFromOptics) where

import           Availability.Getter
import           Availability.Impl
import           Availability.Putter
import           Data.IORef          (IORef)
import           Language.Haskell.TH (Dec, Q, TExp, Type)
import qualified Optics
import           Unsafe.Coerce       (unsafeCoerce)

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

makeStateByIORef :: Q Type -> Q Type -> Q Type -> Q Type -> Q [Dec]
makeStateByIORef tag typ otag mnd =
  [d|
  instance Monad m => Interpret (Getter $tag $typ) $mnd where
    type InTermsOf (Getter $tag $typ) $mnd = '[Getter $otag (IORef $typ), Embed IO]
    {-# INLINABLE unsafeSend #-}
    unsafeSend Get = do
      r <- get @($otag)
      embed $ readIORef r

  instance Monad m => Interpret (Putter $tag $typ) $mnd where
    type InTermsOf (Putter $tag $typ) $mnd = '[Getter $otag (IORef $typ), Embed IO]
    {-# INLINABLE unsafeSend #-}
    unsafeSend (Put x) = do
      r <- get @($otag)
      embed $ writeIORef r x
  |]

makeStateFromOptics :: forall k is s a. Optics.Is k Optics.A_Lens =>
  Q Type -> Q Type -> Q Type -> Q Type -> Q (TExp (Optics.Optic' k is s a)) -> Q Type -> Q [Dec]
makeStateFromOptics tag typ otag otyp lens mnd = concat <$> sequence
  [ makeGetterFromOptics tag typ otag otyp (unsafeCoerce @_ @(Q (TExp (Optics.Optic' Optics.A_Lens is s a))) lens) mnd
  , makePutterFromOptics tag typ otag otyp (unsafeCoerce @_ @(Q (TExp (Optics.Optic' Optics.A_Lens is s a))) lens) mnd
  ]

makeStateKVFromOptics :: Q Type -> Q Type -> Q Type -> Q Type -> Q Type -> Q Type -> Q [Dec]
makeStateKVFromOptics tag k v otag otyp mnd = concat <$> sequence
  [ makeGetterKVFromOptics tag k v otag otyp mnd
  , makePutterKVFromOptics tag k v otag otyp mnd
  , makeDeleterKVFromOptics tag k v otag otyp mnd
  ]
