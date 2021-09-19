module Availability.Reader (Getter (..), get, gets, GetterKV (..), getKV, getsKV, Locally (..), local, reader,
                            makeEffViaMonadReader, makeGetterFromLens, makeLocallyFromLens, makeReaderFromLens,
                            makeGetterKVFromLens) where

import           Availability
import           Control.Monad        (join)
import qualified Control.Monad.Reader as MTL
import           Data.Function        ((&))
import           Language.Haskell.TH  (Dec, Exp, Q, Type)
import           Lens.Micro           ((%~), (^.))
import qualified Lens.Micro           as Lens

data Getter tag s :: Effect where
  Get :: Getter tag s m s

get :: forall tag s m. Sendable (Getter tag s) m => M m s
get = send (Get @tag)
{-# INLINE get #-}

gets :: forall tag s t m. Sendable (Getter tag s) m => (s -> t) -> M m t
gets f = f <$> get @tag
{-# INLINE gets #-}

data GetterKV tag k v :: Effect where
  GetKV :: k -> GetterKV tag k v m (Maybe v)

getKV :: forall tag k v m. Sendable (GetterKV tag k v) m => k -> M m (Maybe v)
getKV k = send (GetKV @_ @tag k)
{-# INLINE getKV #-}

getsKV :: forall tag k v u m. Sendable (GetterKV tag k v) m => (v -> u) -> k -> M m (Maybe u)
getsKV f k = fmap f <$> getKV @tag k
{-# INLINE getsKV #-}

data Locally tag s :: Effect where
  Local :: (s -> s) -> (Eff (Getter tag s) => M m a) -> Locally tag s m a

local :: forall tag s m a. Sendable (Locally tag s) m => (s -> s) -> (Eff (Getter tag s) => M m a) -> M m a
local f m = send (Local @_ @tag f m)
{-# INLINE local #-}

reader :: forall tag s m a. (Sendable (Getter tag s) m) => (s -> a) -> M m a
reader f = f <$> get @tag
{-# INLINE reader #-}

makeGetterFromLens :: Q Type -> Q Type -> Q Type -> Q Type -> Q Exp -> Q Type -> Q [Dec]
makeGetterFromLens tag typ otag otyp lens mnd =
  [d|
  instance Interpret (Getter $tag $typ) $mnd where
    type InTermsOf _ _ = '[Getter $otag $otyp]
    {-# INLINABLE interpret #-}
    interpret Get = do
      s <- get @($otag) @($otyp)
      pure (s ^. $lens)
  |]

makeLocallyFromLens :: Q Type -> Q Type -> Q Type -> Q Type -> Q Exp -> Q Type -> Q [Dec]
makeLocallyFromLens tag typ otag otyp lens mnd =
  [d|
  instance Interpret (Locally $tag $typ) $mnd where
    type InTermsOf _ _ = '[Locally $otag $otyp, Getter $tag $typ]
    {-# INLINE interpret #-}
    interpret (Local f m) = local @($otag) @($otyp) (\x -> x & $lens %~ f) m
  |]

makeReaderFromLens :: Q Type -> Q Type -> Q Type -> Q Type -> Q Exp -> Q Type -> Q [Dec]
makeReaderFromLens tag typ otag otyp lens mnd = join <$> sequence
  [ makeGetterFromLens tag typ otag otyp lens mnd
  , makeLocallyFromLens tag typ otag otyp lens mnd
  ]

makeGetterKVFromLens :: Q Type -> Q Type -> Q Type -> Q Type -> Q Type -> Q Type -> Q [Dec]
makeGetterKVFromLens tag ktyp vtyp otag otyp mnd =
  [d|
  instance Interpret (GetterKV $tag $ktyp $vtyp) $mnd where
    type InTermsOf _ _ = '[Getter $otag $otyp]
    {-# INLINABLE interpret #-}
    interpret (GetKV k) = do
      s <- get @($otag) @($otyp)
      pure (s ^. Lens.at k)
  |]

makeEffViaMonadReader :: Q Type -> Q Type -> Q Type -> Q [Dec]
makeEffViaMonadReader tag typ mnd =
  [d|
  instance Interpret (Getter $tag $typ) $mnd where
    type InTermsOf _ _ = '[Underlying]
    {-# INLINE interpret #-}
    interpret Get = underlie MTL.ask

  instance Interpret (Locally $tag $typ) $mnd where
    type InTermsOf _ _ = '[Underlying]
    {-# INLINE interpret #-}
    interpret (Local f m) = underlie $ MTL.local f (runUnderlying @'[Getter $tag $typ] m)
  |]
