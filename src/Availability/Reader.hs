module Availability.Reader (module Availability.Getter, Locally (..), local, runReader, makeEffViaMonadReader) where

import           Availability.Getter
import           Availability.Impl
import qualified Control.Monad.Reader as MTL
import           Language.Haskell.TH  (Dec, Q, Type)

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