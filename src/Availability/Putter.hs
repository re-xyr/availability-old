module Availability.Putter (Putter (..), put, PutterKV (..), putKV, DeleterKV (..), delKV, makePutterFromOptics,
                            makePutterKVFromOptics, makeDeleterKVFromOptics) where

import           Availability.Getter
import           Availability.Impl
import           Language.Haskell.TH (Dec, Q, TExp (unType), Type)
import           Optics              ((&), (.~))
import qualified Optics

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

makePutterFromOptics :: Optics.Is k Optics.A_Setter =>
  Q Type -> Q Type -> Q Type -> Q Type -> Q (TExp (Optics.Optic' k is s a)) -> Q Type -> Q [Dec]
makePutterFromOptics tag typ otag otyp lens mnd =
  [d|
  instance Interpret (Putter $tag $typ) $mnd where
    type InTermsOf (Putter $tag $typ) $mnd = '[Getter $otag $otyp, Putter $otag $otyp]
    {-# INLINABLE unsafeSend #-}
    unsafeSend (Put x) = do
      s <- get @($otag) @($otyp)
      put @($otag) @($otyp) (s & $(unType <$> lens) .~ x)
  |]

makePutterKVFromOptics :: Q Type -> Q Type -> Q Type -> Q Type -> Q Type -> Q Type -> Q [Dec]
makePutterKVFromOptics tag ktyp vtyp otag otyp mnd =
  [d|
  instance Interpret (PutterKV $tag $ktyp $vtyp) $mnd where
    type InTermsOf (PutterKV $tag $ktyp $vtyp) $mnd = '[Getter $otag $otyp, Putter $otag $otyp]
    {-# INLINABLE unsafeSend #-}
    unsafeSend (PutKV k v) = do
      s <- get @($otag) @($otyp)
      put @($otag) @($otyp) (s & Optics.at k ?~ v)
  |]

makeDeleterKVFromOptics :: Q Type -> Q Type -> Q Type -> Q Type -> Q Type -> Q Type -> Q [Dec]
makeDeleterKVFromOptics tag ktyp vtyp otag otyp mnd =
  [d|
  instance Interpret (DeleterKV $tag $ktyp $vtyp) $mnd where
    type InTermsOf (DeleterKV $tag $ktyp $vtyp) $mnd = '[Getter $otag $otyp, Putter $otag $otyp]
    {-# INLINABLE unsafeSend #-}
    unsafeSend (DelKV k) = do
      s <- get @($otag) @($otyp)
      put @($otag) @($otyp) (Optics.sans k s)
  |]
