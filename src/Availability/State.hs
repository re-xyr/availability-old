module Availability.State (module Availability.Getter, module Availability.Putter, state, modify, makeStateByIORef,
                           makeStateFromLens, makeStateKVFromLens) where

import           Availability.Embed
import           Availability.Getter
import           Availability.Impl
import           Availability.Putter
import           Data.IORef          (IORef)
import           Language.Haskell.TH (Dec, Exp, Q, Type)

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
  instance Interpret (Getter $tag $typ) $mnd where
    type InTermsOf (Getter $tag $typ) $mnd = '[Getter $otag (IORef $typ), Embed IO]
    {-# INLINABLE unsafeSend #-}
    unsafeSend Get = do
      r <- get @($otag)
      embed $ readIORef r

  instance Interpret (Putter $tag $typ) $mnd where
    type InTermsOf (Putter $tag $typ) $mnd = '[Getter $otag (IORef $typ), Embed IO]
    {-# INLINABLE unsafeSend #-}
    unsafeSend (Put x) = do
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
