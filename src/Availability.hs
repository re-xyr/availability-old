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
-- can only be removed all at once, obtaining the underlying monad, via the 'runUnderlying' function:
--
-- @
-- data Ctx = Ctx { _foo :: 'Int', _bar :: 'Data.IORef.IORef' 'Bool' }
-- makeLenses ''Ctx
--
-- type App = 'Control.Monad.Reader.ReaderT' Ctx 'IO'
--
-- testParity :: ('Effs' '['Availability.Reader.Getter' "foo" 'Int', 'Availability.State.Putter' "bar" 'Bool']) => 'M' App ()
-- testParity = do
--   num <- 'Availability.Reader.get' \@"foo" \@'Int'
--   'Availability.State.put' \@"bar" ('even' num)
--
-- example :: 'IO' ()
-- example = do
--   rEven <- 'Data.IORef.newIORef' 'False'
--   'runUnderlying' \@'['Availability.Reader.Getter' "foo" 'Int', 'Availability.State.Putter' "bar" 'Bool'] testParity
--     'Data.Function.&' (\`Control.Monad.Reader.runReaderT\` Ctx 2 rEven)
--   'Data.IORef.readIORef' rEven '>>=' 'print'
-- @
--
-- To provide interpretations to the effects, you can use the predefined TH functions:
--
-- @
-- 'Availability.Embed.makeEffViaMonadIO'                                                                  [t|App|]
-- 'Availability.Reader.makeEffViaMonadReader' [t|"ctx"   |] [t|Ctx       |]                                [t|App|]
-- 'Availability.Reader.makeReaderFromLens'    [t|"foo"   |] [t|'Int'       |] [t|"ctx"   |] [t|Ctx|] [|foo|] [t|App|]
-- 'Availability.Reader.makeReaderFromLens'    [t|"barRef"|] [t|'Data.IORef.IORef' 'Bool'|] [t|"ctx"   |] [t|Ctx|] [|bar|] [t|App|]
-- 'Availability.State.makeStateByIORef'      [t|"bar"   |] [t|'Bool'      |] [t|"barRef"|]                  [t|App|]
-- @
module Availability
  ( -- * The 'M' monad
    M (runM)
  , -- * The phantom 'Eff' constraint
    Effect, Eff, Effs
  , -- * Interpreting effects
    Interpret (..), Interprets, derive, derives
  , -- * Performing effects
    Sendable, send
  , -- * The 'Underlying' pseudo-effect
    Underlying, underlie
  , -- * Running effects
    runUnderlying
  ) where

import           Availability.Internal.Availability
