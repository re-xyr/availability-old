-- | Availability is an unconventional effects library. In traditional effects libraries, users can perform an effect on
-- a @'Monad' m@ if this effect can be interpreted in terms of the monad:
--
-- @
-- f :: (Interpret (Reader A) m, Interpret (State B) m) => m ()
-- @
--
-- While this works well, it has inevitable performance drawback because of the polymorphic @m@. GHC doesn't know
-- the implementation of @m@, hence cannot perform much optimization. In contrast, if one uses a concrete monad 'MyM',
-- that interprets all effects they need, they will not be able to restrict the effects that can be performed.
--
-- Availability addresses this problem by the /phantom constraint pattern/, where an effect @r@ can be performed if and
-- only if:
--
-- - The monad is /capable/ of interpreting this effect,
-- - The effect is /available/ in current context, i.e. a phantom constraint @'Eff' r@,
--   which doesn't contain any information, is in the context.
--
-- This pattern is outlined in the blog post [/Effect is a phantom/](https://喵.世界/2021/09/14/redundant-constraints/).
-- In Availability, the usage of this pattern allows you to control the effect via the phantom 'Eff' constraint, while
-- using a concrete monad, because the phantom constraint 'Eff' is not tied to the monad. 'Eff' has no instances, and
-- can only be removed all at once, obtaining the underlying monad, via the 'runM' function:
--
-- @
-- data Ctx = Ctx { foo :: 'Int', bar :: 'Data.IORef.IORef' 'Bool' } deriving ('GHC.Generics.Generic')
--
-- newtype App = App { runApp :: 'Control.Monad.Reader.ReaderT' Ctx 'IO' }
--
-- testParity :: ('Effs' '['Availability.Reader.Getter' "foo" 'Int', 'Availability.State.Putter' "bar" 'Bool']) => 'M' App ()
-- testParity = do
--   num <- 'Availability.Reader.get' \@"foo" \@'Int'
--   'Availability.State.put' \@"bar" ('even' num)
--
-- example :: 'IO' ()
-- example = do
--   rEven <- 'Data.IORef.newIORef' 'False'
--   'runM' \@'['Availability.Reader.Getter' "foo" 'Int', 'Availability.State.Putter' "bar" 'Bool'] testParity
--     'Data.Function.&' runApp
--     'Data.Function.&' (\`'Control.Monad.Reader.runReaderT'\` Ctx 2 rEven)
--   'Data.IORef.readIORef' rEven '>>=' 'print'
-- @
--
-- To provide interpretations to the effects, you can use the predefined /interpretation strategies/, i.e. newtypes
-- that extends the monad with a few insteances, intended to be used with @DerivingVia@. You should attach the
-- @deriving via@ clauses to the concrete monad definition:
--
-- @
-- newtype App = App { runApp :: 'Control.Monad.Reader.ReaderT' Ctx 'IO' }
--   deriving ('Functor', 'Applicative', 'Monad', 'Control.Monad.Trans.MonadIO', 'Control.Monad.Reader.MonadReader' Ctx)
--   deriving ('Interpret' 'Availability.Embed.Embed' 'IO')
--     via 'Availability.Embed.ViaMonadIO' App
--   deriving ('Interpret' 'Availability.Reader.Getter' "ctx" Ctx)
--     via 'Availability.Reader.VIaMonadReader' App
--   deriving ('Interpret' 'Availability.Reader.Getter' "foo" Int)
--     via 'Availability.Lens.FromHas' "foo" "ctx" Ctx App
--   deriving ('Interpret' 'Availability.State.Putter' "bar" Bool)
--     via 'Availability.State.StateByIORef' () ('Availability.Lens.FromHas' "bar" "ctx" Ctx App)
-- @
module Availability
  ( -- * The 'M' monad
    M, coerceM, coerceM'
  , -- * The phantom 'Eff' constraint
    Effect, Eff, Effs
  , -- * Interpreting effects
    Interpret (..), Interprets
  , -- * Performing effects
    Sendable, send, Sendables
  , -- * The 'Underlying' pseudo-effect
    Underlying, underlie
  , -- * Running effects
    runM, runM'
  ) where

import           Availability.Internal.Availability
