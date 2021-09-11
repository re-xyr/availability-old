module Availability.Internal.Availability where

import           Data.Kind     (Constraint)
import           Unsafe.Coerce (unsafeCoerce)

newtype M (m :: * -> *) a = UnsafeLift { runM :: m a }
  deriving (Functor, Applicative, Monad)

type Effect = (* -> *) -> * -> *

data UnsafeEffProxy r = UnsafeEffProxy

class Eff (r :: Effect) where
  unsafeInstEffect :: UnsafeEffProxy r

type family Effs (rs :: [Effect]) :: Constraint where
  Effs '[] = ()
  Effs (x ': xs) = (Eff x, Effs xs)

-- rip is NOT safe!
rip :: forall r a. (Eff r => a) -> a
rip m = unsafeCoerce @(UnsafeInterpretWrapper r a) @(UnsafeEffProxy r -> a) (UnsafeInterpretWrapper m) UnsafeEffProxy
{-# INLINE rip #-}

newtype UnsafeInterpretWrapper r a = UnsafeInterpretWrapper (Eff r => a)

class Rip (k :: [Effect]) where
  -- rips is NOT safe!
  rips :: (Effs k => a) -> a

instance Rip '[] where
  rips x = x
  {-# INLINE rips #-}

instance Rip rs => Rip (r ': rs) where
  rips x = rip @r (rips @rs x)
  {-# INLINABLE rips #-}

type family Interprets (xs :: [Effect]) (m :: * -> *) :: Constraint where
  Interprets '[] _ = ()
  Interprets (x ': xs) m = (Interpret x m, Interprets (InTermsOf x m) m, Interprets xs m)

class (Rip (InTermsOf r m)) => Interpret r m where
  {-# MINIMAL unsafeSend #-}
  type InTermsOf r m :: [Effect]
  unsafeSend :: (Interprets '[r] m, Effs (InTermsOf r m)) => r m a -> M m a

-- send is safe in the sense that it restricts the effect needed to be in scope.
send :: forall r m a. Sendable r m => r m a -> M m a
send = rips @(InTermsOf r m) unsafeSend
{-# INLINE send #-}

-- interpret is safe in the sense that it requires the interpreter to be in place.
interpret :: forall r m a. Interpret r m => (Eff r => M m a) -> (Effs (InTermsOf r m) => M m a)
interpret = rip @r
{-# INLINE interpret #-}

type Sendable r m = (Eff r, Interprets '[r] m)

data Underlying :: Effect

instance Interpret Underlying m where
  type InTermsOf Underlying m = '[]
  unsafeSend = \case

-- underlie is safe in the sense that only those with the Underlying effect can perform it.
underlie :: Eff Underlying => m s -> M m s
underlie = UnsafeLift
{-# INLINE underlie #-}

-- runUnderlying is safe in the sense that one cannot wrap the unwrapped action back again.
runUnderlying :: forall r m s. Rip r => ((Eff Underlying, Effs r) => M m s) -> m s
runUnderlying m = runM (rips @r (rip @Underlying m))
{-# INLINE runUnderlying #-}

data Embed (m' :: * -> *) :: Effect where
  Embed :: m' a -> Embed m' m a

embed :: forall m' m a. Sendable (Embed m') m => m' a -> M m a
embed m = send (Embed m)
{-# INLINE embed #-}
