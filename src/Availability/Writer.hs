module Availability.Writer (Teller (..), tell, Listener (..), listen, pass, ViaMonadWriter (..), TellerByList (..),
                            TellerByMonoid (..)) where

import           Availability
import           Availability.Lens
import           Availability.State   (Getter, Putter, modify')
import           Control.Lens         ((#))
import           Control.Monad.Trans  (MonadIO)
import qualified Control.Monad.Writer as MTL
import           Data.Generics.Sum    (AsAny (_As))

data Teller tag w :: Effect where
  Tell :: w -> Teller tag w m ()

tell :: forall tag w m. (Sendable (Teller tag w) m) => w -> M m ()
tell x = send (Tell @w @tag x)
{-# INLINE tell #-}

data Listener tag w :: Effect where
  Listen :: M m a -> Listener tag w m (a, w)
  Pass :: M m (a, w -> w) -> Listener tag w m a

listen :: forall tag w m a. (Sendable (Listener tag w) m) => M m a -> M m (a, w)
listen m = send (Listen @m @a @tag m)
{-# INLINE listen #-}

pass :: forall tag w m a. (Sendable (Listener tag w) m) => M m (a, w -> w) -> M m a
pass m = send (Pass @m @a @w @tag m)
{-# INLINE pass #-}

newtype ViaMonadWriter m a = ViaMonadWriter (m a)
  deriving (Functor, Applicative, Monad, MonadIO, MTL.MonadWriter w)

instance MTL.MonadWriter w m => Interpret (Teller tag w) (ViaMonadWriter m) where
  type InTermsOf _ _ = '[Underlying]
  {-# INLINE interpret #-}
  interpret (Tell x) = underlie $ MTL.tell x

instance MTL.MonadWriter w m => Interpret (Listener tag w) (ViaMonadWriter m) where
  type InTermsOf _ _ = '[Underlying]
  {-# INLINE interpret #-}
  interpret (Listen m) = underlie $ MTL.listen (runM' m)
  interpret (Pass m)   = underlie $ MTL.pass (runM' m)

newtype TellerByList otag m a = TellerByList (m a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance Interprets '[Getter otag [w], Putter otag [w]] m => Interpret (Teller tag w) (TellerByList otag m) where
  type InTermsOf _ _ = '[Getter otag [w], Putter otag [w]]
  {-# INLINE interpret #-}
  interpret (Tell x) = coerceM @m $ modify' @otag (x :)

newtype TellerByMonoid otag m a = TellerByMonoid (m a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Monoid w, Interprets '[Getter otag w, Putter otag w] m) =>
  Interpret (Teller tag w) (TellerByMonoid otag m) where
  type InTermsOf _ _ = '[Getter otag w, Putter otag w]
  {-# INLINE interpret #-}
  interpret (Tell x) = coerceM @m $ modify' @otag (<> x)

instance (Interprets '[Teller otag w] m, AsAny sel v w) => Interpret (Teller tag v) (FromAs sel otag w m) where
  type InTermsOf _ _ = '[Teller otag w]
  {-# INLINE interpret #-}
  interpret (Tell x) = coerceM @m $ tell @otag @w (_As @sel # x)

-- instance Interpret (Listener $tag $typ) $mnd where
--   type InTermsOf _ _ = '[Getter $otag $typ, Putter $otag $typ]
--   interpret (Listen m) = do
--     s <- get @($otag) @($typ)
--     put @($otag) @($typ) mempty
--     x <- m
--     s' <- get @($otag)
--     put @($otag) $! s <> s'
--     pure (x, s')
--   interpret (Pass m) = do
--     s <- get @($otag) @($typ)
--     put @($otag) @($typ) mempty
--     (x, f) <- m
--     s' <- get @($otag)
--     put @($otag) $! s <> f s'
--     pure x
