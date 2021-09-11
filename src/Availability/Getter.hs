module Availability.Getter (Getter (..), get, GetterKV (..), getKV, makeGetterFromOptics, makeGetterKVFromOptics) where

import           Availability.Impl
import           Language.Haskell.TH (Dec, Q, TExp (unType), Type)
import           Optics              ((^.))
import qualified Optics

data Getter tag s :: Effect where
  Get :: Getter tag s m s

get :: forall tag s m. Sendable (Getter tag s) m => M m s
get = send (Get @tag)
{-# INLINE get #-}

data GetterKV tag k v :: Effect where
  GetKV :: k -> GetterKV tag k v m (Maybe v)

getKV :: forall tag k v m. Sendable (GetterKV tag k v) m => k -> M m (Maybe v)
getKV k = send (GetKV @_ @tag k)
{-# INLINE getKV #-}

makeGetterFromOptics :: Optics.Is k Optics.A_Getter =>
  Q Type -> Q Type -> Q Type -> Q Type -> Q (TExp (Optics.Optic' k is s a)) -> Q Type -> Q [Dec]
makeGetterFromOptics tag typ otag otyp lens mnd =
  [d|
  instance Interpret (Getter $tag $typ) $mnd where
    type InTermsOf (Getter $tag $typ) $mnd = '[Getter $otag $otyp]
    {-# INLINABLE unsafeSend #-}
    unsafeSend Get = do
      s <- get @($otag) @($otyp)
      pure (s ^. $(unType <$> lens))
  |]

makeGetterKVFromOptics :: Q Type -> Q Type -> Q Type -> Q Type -> Q Type -> Q Type -> Q [Dec]
makeGetterKVFromOptics tag ktyp vtyp otag otyp mnd =
  [d|
  instance Interpret (GetterKV $tag $ktyp $vtyp) $mnd where
    type InTermsOf (GetterKV $tag $ktyp $vtyp) $mnd = '[Getter $otag $otyp]
    {-# INLINABLE unsafeSend #-}
    unsafeSend (GetKV k) = do
      s <- get @($otag) @($otyp)
      pure (s ^. Optics.at k)
  |]
