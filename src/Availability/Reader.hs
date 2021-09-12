module Availability.Reader (Getter (..), get, GetterKV (..), getKV, Locally (..), local, runReader,
                            makeEffViaMonadReader, makeGetterFromLens, makeGetterKVFromLens) where

import           Availability.Impl
import qualified Control.Monad.Reader as MTL
import           Language.Haskell.TH  (Dec, Exp, Q, Type)
import           Lens.Micro           ((^.))
import qualified Lens.Micro           as Lens

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

data Locally tag s :: Effect where
  Local :: (s -> s) -> M m a -> Locally tag s m a

local :: forall tag s m a. Sendable (Locally tag s) m => (s -> s) -> M m a -> M m a
local f m = send (Local @_ @_ @_ @tag f m)
{-# INLINE local #-}

-- runReader is magical, but safe.
runReader :: forall tag s m a. Sendable (Locally tag s) m =>
  s -> (Effs '[Getter tag s, Locally tag s] => M m a) -> M m a
runReader s m = rips @'[Getter tag s, Locally tag s] $ local @tag (const s) m
{-# INLINE runReader #-}

makeGetterFromLens :: Q Type -> Q Type -> Q Type -> Q Type -> Q Exp -> Q Type -> Q [Dec]
makeGetterFromLens tag typ otag otyp lens mnd =
  [d|
  instance Interpret (Getter $tag $typ) $mnd where
    type InTermsOf (Getter $tag $typ) $mnd = '[Getter $otag $otyp]
    {-# INLINABLE unsafeSend #-}
    unsafeSend Get = do
      s <- get @($otag) @($otyp)
      pure (s ^. $lens)
  |]

makeGetterKVFromLens :: Q Type -> Q Type -> Q Type -> Q Type -> Q Type -> Q Type -> Q [Dec]
makeGetterKVFromLens tag ktyp vtyp otag otyp mnd =
  [d|
  instance Interpret (GetterKV $tag $ktyp $vtyp) $mnd where
    type InTermsOf (GetterKV $tag $ktyp $vtyp) $mnd = '[Getter $otag $otyp]
    {-# INLINABLE unsafeSend #-}
    unsafeSend (GetKV k) = do
      s <- get @($otag) @($otyp)
      pure (s ^. Lens.at k)
  |]

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
