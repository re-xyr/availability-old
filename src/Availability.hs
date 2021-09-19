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
-- data Global = Global
--   { _myA :: A
--   , _myB :: IORef B
--   } deriving (Generic)
-- makeLenses ''Global
--
-- type App = ReaderT Global IO
--
-- f :: 'Eff' '['Availability.Reader.Getter' "myA" A, 'Availability.Reader.Getter' "myB" B, 'Availability.State.Putter' "myB" B] => 'M' App ()
-- f = do
--   x <- 'Availability.Reader.get' \@"myA"
--   y <- 'Availability.Reader.get' \@"myB"
--   'Availability.State.put' \@"myB" (doSomething x y)
--   ...
--
-- main :: IO ()
-- main = do
--   refB <- newIORef someB
--   f & 'runUnderlying' @'['Availability.Reader.Getter' "myA" A, 'Availability.Reader.Getter' "myB" B, 'Availability.State.Putter' "myB" B]
--     & (\`runReaderT\` Global someA refB)
-- @
--
-- To provide interpretations to the effects, you can use the predefined TH functions:
--
-- @
-- 'Availability.Reader.makeEffViaMonadReader' [t| "global" |] [t| Global  |]                                         [t| App |]
-- 'Availability.Reader.makeGetterFromLens'    [t| "myA"    |] [t| A       |] [t| "global" |] [t| Global |] [| myA |] [t| App |]
-- 'Availability.Reader.makeGetterFromLens'    [t| "refB"   |] [t| IORef B |] [t| "global" |] [t| Global |] [| myB |] [t| App |]
-- 'Availability.State.makeStateFromIORef'    [t| "myB"    |] [t| B       |]                                         [t| App |]
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
