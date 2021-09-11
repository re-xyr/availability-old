module Availability.Writer (module Availability.Putter, Listener (..), listen, pass, makeEffViaMonadWriter) where

import           Availability.Impl
import           Availability.Putter
import qualified Control.Monad.Writer as MTL
import           Language.Haskell.TH

data Listener tag w :: Effect where
  Listen :: M m a -> Listener tag w m (a, w)
  Pass :: M m (a, w -> w) -> Listener tag w m a

listen :: forall tag w m a. (Sendable (Listener tag w) m) => M m a -> M m (a, w)
listen m = send (Listen @m @a @tag m)
{-# INLINE listen #-}

pass :: forall tag w m a. (Sendable (Listener tag w) m) => M m (a, w -> w) -> M m a
pass m = send (Pass @m @a @w @tag m)
{-# INLINE pass #-}

makeEffViaMonadWriter :: Q Type -> Q Type -> Q Type -> Q [Dec]
makeEffViaMonadWriter tag typ mnd =
  [d|
  instance Interpret (Putter $tag $typ) $mnd where
    type InTermsOf (Putter $tag $typ) $mnd = '[Underlying]
    {-# INLINE unsafeSend #-}
    unsafeSend (Put x) = underlie $ MTL.tell x

  instance Interpret (Listener $tag $typ) $mnd where
    type InTermsOf (Listener $tag $typ) $mnd = '[Underlying]
    {-# INLINE unsafeSend #-}
    unsafeSend (Listen m) = underlie $ MTL.listen (runM m)
    unsafeSend (Pass m)   = underlie $ MTL.pass (runM m)
  |]
