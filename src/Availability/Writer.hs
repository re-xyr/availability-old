module Availability.Writer (Teller (..), tell, Listener (..), listen, pass, makeEffViaMonadWriter,
                            makeTellerByList, makeTellerByMonoid) where

import           Availability
import           Availability.State   (Getter, Putter, modify')
import qualified Control.Monad.Writer as MTL
import           Language.Haskell.TH

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

makeEffViaMonadWriter :: Q Type -> Q Type -> Q Type -> Q [Dec]
makeEffViaMonadWriter tag typ mnd =
  [d|
  instance Interpret (Teller $tag $typ) $mnd where
    type InTermsOf _ _ = '[Underlying]
    {-# INLINE interpret #-}
    interpret (Tell x) = underlie $ MTL.tell x

  instance Interpret (Listener $tag $typ) $mnd where
    type InTermsOf _ _ = '[Underlying]
    {-# INLINE interpret #-}
    interpret (Listen m) = underlie $ MTL.listen (runM m)
    interpret (Pass m)   = underlie $ MTL.pass (runM m)
  |]

makeTellerByList :: Q Type -> Q Type -> Q Type -> Q Type -> Q [Dec]
makeTellerByList tag typ otag mnd =
  [d|
  instance Interpret (Teller $tag $typ) $mnd where
    type InTermsOf _ _ = '[Getter $otag [$typ], Putter $otag [$typ]]
    interpret (Tell x) = modify' @() (x :)
  |]

-- Note that Listener is not thread safe in this instance.
makeTellerByMonoid :: Q Type -> Q Type -> Q Type -> Q Type -> Q [Dec]
makeTellerByMonoid tag typ otag mnd =
  [d|
  instance Interpret (Teller $tag $typ) $mnd where
    type InTermsOf _ _ = '[Getter $otag $typ, Putter $otag $typ]
    {-# INLINE interpret #-}
    interpret (Tell x) = modify' @($otag) (<> x)
  |]

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
