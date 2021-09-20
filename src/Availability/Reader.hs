module Availability.Reader (Getter (..), get, gets, GetterKV (..), getKV, getsKV, Locally (..), local, reader,
                            ViaMonadReader (..)) where

import           Availability
import           Availability.Embed
import           Availability.Lens
import           Control.Lens          (At (at), Index, IxValue, (%~), (^.))
import qualified Control.Monad.Reader  as MTL
import           Control.Monad.Trans   (MonadIO)
import           Data.Function         ((&))
import           Data.Generics.Product (HasAny (the))

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

instance (Interprets '[Getter otag r] m, HasAny sel r r s s) => Interpret (Getter tag s) (FromHas sel otag r m) where
  type InTermsOf _ _ = '[Getter otag r]
  {-# INLINABLE interpret #-}
  interpret Get = coerceM @m $ do
    r <- get @otag @r
    pure (r ^. the @sel)

instance (Interprets '[Locally otag r, Getter tag s] m, HasAny sel r r s s) =>
  Interpret (Locally tag s) (FromHas sel otag r m) where
  type InTermsOf _ _ = '[Locally otag r, Getter tag s]
  interpret (Local f m) = coerceM @m $ local @otag @r (\x -> x & (the @sel) %~ f) (coerceM' @m m)

instance (Interprets '[Getter otag r] m, At r, k ~ Index r, v ~ IxValue r) =>
  Interpret (GetterKV tag k v) (FromAt otag r m) where
  type InTermsOf _ _ = '[Getter otag r]
  interpret (GetKV k) = coerceM @m do
    r <- get @otag @r
    pure (r ^. at k)

newtype ViaMonadReader m a = ViaMonadReader (m a)
  deriving (Functor, Applicative, Monad, MonadIO, MTL.MonadReader r)
deriving instance Interpret (Embed IO) m => Interpret (Embed IO) (ViaMonadReader m)

instance MTL.MonadReader r m => Interpret (Getter tag r) (ViaMonadReader m) where
  type InTermsOf _ _ = '[Underlying]
  {-# INLINE interpret #-}
  interpret Get = underlie MTL.ask

instance MTL.MonadReader r m => Interpret (Locally tag r) (ViaMonadReader m) where
  type InTermsOf _ _ = '[Underlying]
  {-# INLINE interpret #-}
  interpret (Local f m) = underlie $ MTL.local f (runM @'[Getter tag r] m)
