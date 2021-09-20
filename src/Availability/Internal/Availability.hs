{-# OPTIONS_HADDOCK not-home #-}
-- | This module is the "core" of Availability's effect system and every definition, whether safe or unsafe, is
-- exported. You don't typically need to use this module; the "Availability" module re-exports all safe definitions
-- from here, and is sufficient for most usage.
module Availability.Internal.Availability where

import           Data.Coerce   (Coercible, coerce)
import           Data.Kind     (Constraint)
import           Data.Proxy    (Proxy (Proxy))
import           Unsafe.Coerce (unsafeCoerce)

-- * The 'M' monad

-- | To restrict the effects that can be performed, this monad is used to wrap the concrete monad @m@ everywhere.
-- Users are therefore screened from directly manipulating the underlying monad @m@.
--
-- Specifically, the constructor of 'M' is not exported because 'Data.Coerce.coerce'ing from @m@ to @'M' m@ is
-- /unsafe w.r.t. effects/. However, coercing from any @'M' m@ to @'M' n@ with @'Data.Coerce.Coercible' m n@ is
-- generally safe and can be conveniently done via the utility function 'coerceM'.
newtype M m a = UnsafeLiftM -- ^ Unsafely lift an @m a@ to @'M' m a@.
  { unM :: m a
    -- ^ Unwrap and obtain the inner monad. This is safe, but the function synonym 'runM'' is exported instead for
    -- documentation clarity.
  } deriving (Functor, Applicative, Monad)

-- | Coerce between underlying monads that are 'Data.Coerce.Coercible' to each other. Practically, this means coercing
-- to and from newtypes over monads. This function is especially useful in creating "interpretation strategies", /i.e./
-- newtype wrappers that extends a monad with a few instances, intended to be used with @DerivingVia@:
--
-- @
-- newtype GetterFromIORef otag m a = GetterFromIORef (m a)
--   deriving ('Functor', 'Applicative', 'Monad')
--
-- instance 'Interprets' '['Availability.Reader.Getter' otag ('Data.IORef.IORef' a), 'Availability.Embed.Embed' 'IO'] m => 'Interpret' ('Availability.Reader.Getter' tag a) m where
--   type 'InTermsOf' _ _ = '['Availability.Reader.Getter' otag ('Data.IORef.IORef' a), 'Availability.Embed.Embed' 'IO']
--   'interpret' 'Availability.Reader.Get' = 'coerceM' \@m do
--     ref <- 'Availability.Reader.get' \@otag
--     'Availability.Embed.embed' $ 'Data.IORef.readIORef' ref
-- @
coerceM :: forall m n a. Coercible m n => M m a -> M n a
coerceM = coerce
{-# INLINE coerceM #-}

-- | 'coerceM' with the order of the two monad type variables flipped.
coerceM' :: forall n m a. Coercible m n => M m a -> M n a
coerceM' = coerce
{-# INLINE coerceM' #-}

-- * The 'Eff' phantom constraint

-- | The kind of (higher order) effects.
type Effect = (* -> *) -> * -> *

-- -- | This datatype is /only/ used in 'rip' to rip off an 'Eff' phantom constraint. We define this instead of using
-- -- 'Data.Proxy.Proxy' because we do not want users to gain access to this dataype. You should not be using this
-- -- datatype anywhere anyhow.
-- data UnsafeEffProxy r = UnsafeEffProxy

-- | The implementation of 'Eff' phantom constraint, containing no information. This current implementation takes
-- advantage of the "[reflection trick](https://stackoverflow.com/questions/17793466)", where @'Eff'' r@ and
-- @'Proxy' r@ can be safely 'unsafeCoerce'd to each other. This class should not be directly used nor
-- instantiated anyhow.
class Eff' (r :: Effect) where
  unsafeInstEffect :: Proxy r

-- | A phantom constraint that indicates an effect is available. For any 'Effect' @r@, @'Eff' r@ has to be in the
-- context in order to 'send' the effect's operations.
--
-- In general, to perform an effect @r@ (via 'send') on a monad @'M' m@, these two requirements have to be satisfied:
--
-- - The monad is /capable/ of interpreting the effect, /i.e./ there must be an instance of @'Interpret' r m@,
-- - The effect is /available/ in current context, /i.e./ @'Eff' r@ must be in current context.
--
-- This twofold requirement allows restricting effects on a concrete monad, and is called the
-- /phantom constraint pattern/. This pattern is outlined in the blog post
-- [/Effect is a phantom/](https://喵.世界/2021/09/14/redundant-constraints/).
type Eff r = Eff' r

-- | Brutally rip off an effect constraint. This function is /very unsafe/, as it can make an effectful action no
-- longer have an 'Eff' constraint, and you should probably not directly use it in most situations.
--
-- It is, however, a very primitive building block used in the definition of safe primitives like 'send' and 'derive',
-- and you may use it in building some other safe primitives, but /be very careful/.
rip :: forall r a. (Eff r => a) -> a
rip m = unsafeCoerce @(UnsafeRipWrapper r a) @(Proxy r -> a) (UnsafeRipWrapper m) Proxy
{-# INLINE rip #-}

-- | This newtype is basically equal to the type @'Eff' r => a@, except that GHC will be more lenient on this newtype,
-- necessary for the reflection trick in 'rip'. It doesn't and shouldn't have any other use anyhow.
newtype UnsafeRipWrapper r a = UnsafeRipWrapper (Eff r => a)

-- | Convenient constraint alias for @('Eff' r1, ..., 'Eff' rn)@.
type family Effs (rs :: [Effect]) :: Constraint where
  Effs '[] = ()
  Effs (r ': rs) = (Eff r, Effs rs)

-- | Typeclass for ripping off many effect constraints at once. This typeclass has instance for every concrete type
-- level list.
class Rip rs where
  -- | Butely 'rip' off many effect constraints. This function is /very unsafe/, and you should probably not use
  -- it. See 'rip'.
  rips :: (Effs rs => a) -> a

instance Rip '[] where
  rips x = x
  {-# INLINE rips #-}

instance Rip rs => Rip (r ': rs) where
  rips x = rip @r (rips @rs x)
  {-# INLINABLE rips #-}

-- * Interpreting effects

-- | The interpretation, or /semantics/, of an effect @r@ in terms of the monad @'M' m@.
--
-- The effect being interpreted can use a set of other more primitive effects by specifying them in the associated type
-- @'InTermsOf' r m@.
class (Monad m, Rip (InTermsOf r m)) => Interpret r m where
  {-# MINIMAL interpret #-}

  -- | The more primitive effects that @r@ is interpreted into.
  --
  -- Eventually, the dependency relation among the effects, formed by 'InTermsOf', typically should form an acyclic
  -- graph (DAG). This may not be true in some situations, most notably the 'Underlying' pseudo-effect; however most
  -- other effects should follow this principle as it provides convenience for using funcitons like 'derive'.
  type InTermsOf r m :: [Effect]

  -- | Interpret an effect @r@ in terms of more primitive effects @'InTermsOf' r m@ in the monad @'M' m@.
  interpret :: Effs (InTermsOf r m) => r m a -> M m a

-- | Converts the effect constraint @r@ into its underlying effects @'InTermsOf' r m@, so that the effect can be
-- performed where only the 'Eff' constraints for the underlying effects are in the context:
--
-- @
-- instace 'Interpret' ('Availability.Reader.Getter' "myA" A) MyM where
--   type 'InTermsOf' _ _ = '['Availability.Reader.Getter' "refA" ('Data.IORef.IORef' A), 'Availability.Embed.Embed' 'IO']
--   ...
--
-- f :: 'Effs' '['Availability.Reader.Getter' "refA" ('Data.IORef.IORef' A), 'Availability.Embed.Embed' 'IO'] => 'M' MyM A
-- f = 'derive' \@('Availability.Reader.Getter' "myA" A) $ 'Availability.Reader.get' \@"myA"
-- @
derive :: forall r m a. Interpret r m => (Eff r => M m a) -> (Effs (InTermsOf r m) => M m a)
derive = rip @r
{-# INLINE derive #-}

-- | Convenient alias constraint for @('Interpret' x1 m, ..., 'Interpret' xn m)@.
type family Interprets rs m :: Constraint where
  Interprets '[] _ = ()
  Interprets (r ': rs) m = (Interpret r m, Interprets rs m)

-- | Type level list concatenation.
type family xs ++ ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- | Convenient alias for @'InTermsOf' r1 m 'Availability.Internal.Availability.++' ... 'Availability.Internal.Availability.++' 'InTermsOf' rn m@.
type family InTermsOfs rs m where
  InTermsOfs '[] m = '[]
  InTermsOfs (r ': rs) m = InTermsOf r m ++ InTermsOfs rs m

-- | Converts multiple effects @rs@ into their underlying effects @'InTermsOfs' rs m@, so that the effects can be
-- performed where only the 'Eff' constraints for the underlying effects are in the context. See 'derive' for the
-- single-effect version.
derives :: forall rs m a. (Rip rs, Interprets rs m) => (Effs rs => M m a) -> (Effs (InTermsOfs rs m) => M m a)
derives = rips @rs
{-# INLINE derives #-}

-- * Performing effects

-- | A convenient alias constraint for @('Eff' r, 'Interpret' r m)@, /i.e./ satisfying the two requirements of
-- performing an effect 'r' on monad 'm'.
type Sendable r m = (Eff r, Interpret r m)

-- | Perform an effect in the monad, given the 'Eff' constraint is in the context, and the effect can be interpreted
-- in terms of the monad. This is the basic way how you do effectful operations.
--
-- It is common for each effect to define convenient alias for @'send' (SomeEffect ...)@:
--
-- @
-- someEffect ... = 'send' (SomeEffect ...)
-- @
send :: forall r m a. Sendable r m => r m a -> M m a
send = rips @(InTermsOf r m) interpret
{-# INLINE send #-}

-- | A convenient alias constraint for @('Sendable r1 m', ..., 'Sendable' rn m)@.
type family Sendables rs m :: Constraint where
  Sendables '[] _ = ()
  Sendables (r ': rs) m = (Sendable r m, Sendables rs m)

-- * The 'Underlying' pseudo-effect

-- | 'Underlying' is a pseudo-effect with no operations. Every monad interprets 'Underlying'. The only use of
-- this effect is that if @'Eff' 'Underlying'@ is in the context, The user can use 'underlie' to embed
-- @m a@ into @'M' m a@.
--
-- 'Underlying' is the "strongest" effect; it allows the user to freely manipulate the underlying monad. Many effects
-- are interpreted in terms of it, /i.e./ directly mapping effects to underlying monad operations.
-- For example, for any @'Control.Monad.Reader.MonadReader' r m@, one can interpret the 'Availability.Reader.Getter'
-- effect directly to 'Control.Monad.Reader.get' via 'Underlying':
--
-- @
-- instance 'Control.Monad.Reader.MonadReader' r m => 'Interpret' ('Availability.Reader.Getter' "foo" r) m where
--   type 'InTermsOf' _ _ = '['Underlying']
--   interpret 'Availability.Reader.Get' = 'underlie' 'Control.Monad.Reader.get'
-- @
--
-- 'Underlying' is also the /terminal/ effect; all effects are eventually interpreted into the 'Underlying' effect
-- (i.e. the underlying monad itself).
data Underlying :: Effect

-- | Every monad interprets the 'Underlying' effect in terms of itself, so that @'derive' \@'Underlying'@ is a no-op.
instance Monad m => Interpret Underlying m where
  type InTermsOf Underlying m = '[Underlying]
  interpret = \case

-- | Embed the underlying monad @m@ into @'M' m@, given the 'Underlying' pseudo-effect is in the context.
underlie :: Eff Underlying => m a -> M m a
underlie = UnsafeLiftM
{-# INLINE underlie #-}

-- * Running effects

-- | Unwrap an effectful computation into its underlying monad, discarding all the 'Eff' constraints.
--
-- In Availability, effects are detached from the underlying monad, which means that /effects imply nothing about/
-- /the monad structure/. This means that an effectful computation can only be run, or /unwrapped/, into its underlying
-- monad as a whole.
runM :: forall rs m a. Rip rs => ((Effs rs) => M m a) -> m a
runM m = unM (rips @rs m)
{-# INLINE runM #-}

-- | Unwrap a pure computation into its underlying monad, /i.e./ 'runM' without discarding 'Eff' constraints.
runM' :: forall m a. M m a -> m a
runM' = unM
{-# INLINE runM' #-}
