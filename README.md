# availability

`availability` is an unconventional effects library.

- Effects are small, to provide fine-grained effect control: `Reader` = `Getter` + `Locally`, `State` = `Getter` + `Putter`, `Error` = `Thrower` + `Catcher`...
- Effectful functions use a user-provided fixed monad instead of a universal `Eff` or a variable monad type.

## Rationale

Although there are many interpretations to one effect, in most cases only one is used per effect per project. Since the interpretation is locally *fixed* in this sense, the underlying monad running the effects could also be fixed. By fixing a concrete monad, there is a lot more space for compiler optimization based on implementation.

Current effect libraries all use either a universal `Eff` monad that has the capability of offering a wide range of effects, or just be completely polymorphic on the underlying monad to delay any handling of implementation details. `availability` instead asks user to define one concrete monad that can supply all effects used in current project, be it a `ReaderT Global IO` or a traditional transformer stack.

We use a newtype wrapper `M` to screen out the user from directly manipulating the underlying monad, and let user to implement interpretations of effects on `M UnderlyingMonad` in terms of other effects or the underlying monad. Although an `M UnderlyingMonad` offer interpretations of all effects used in a project, `availability` provides a way to express and restrict what effects are used by different effectul functions.

```haskell
instance Interpret MyEff UnderlyingMonad where
  ...

myLogic :: Eff MyEff => M UnderlyingMonad a
myLogic = ...
```

Here, the `Interpret MyEff UnderlyingMonad` instance contains the implementation of a certain effect on the underlying monad. However users must add a phantom constraint `Eff MyEff` in any function using this interpreted effect, otherwise they face a type error. The typeclass `Eff` does not contain any information itself, and it is purely restrictional. This expresses a separation of concern: the restrictional *availability*, i.e. the `Eff` phantom constraint, as opposed to the implementational *capability*, i.e. the `Interpret` typeclass.

## Performance

`availability` has good performance and performed better than `fused-effects`, `freer-simple`, `polysemy` and sometimes `mtl` in `effect-zoo` microbenchmarks.

### `big-stack`

This benchmark interprets multiple layers of no-op effects. `availability` performed almost identical to `mtl`. This is because I used `mtl` to build the underlying monad.

![big-stack benchmark result](https://raw.githubusercontent.com/re-xyr/availability/master/docs/img/big-stack.png)

### `countdown`

This benchmark decrements a counter till 0. `availability` performed identical to reference implementation due to GHC optimization, even after separating effect implementations and the program.

![countdown benchmark result](https://raw.githubusercontent.com/re-xyr/availability/master/docs/img/countdown.png)

### `file-sizes`

This benchmark tests a typical practical scenario of reading files and logging. `availability` has slightly worse performance than `mtl` and slightly better than `fused-effects`.

![file-sizes benchmark result](https://raw.githubusercontent.com/re-xyr/availability/master/docs/img/file-sizes.png)

### `reinterpretation`

This benchmark involves reinterpreting higher level effects to more primitive ones. `availability` performed better than other libraries.

![reinterpretation benchmark result](https://raw.githubusercontent.com/re-xyr/availability/master/docs/img/reinterpretation.png)

## Example

This is the definition of the classic `Teletype` effect in `availability`.

```haskell
import Availability

data Teletype :: Effect where
  ReadTTY :: Teletype m String
  WriteTTY :: String -> Teletype m ()

readTTY :: Sendable Teletype m => M m String
readTTY = send ReadTTY

writeTTY :: Sendable Teletype m => String -> M m ()
writeTTY s = send (WriteTTY s)
```

One can implement a pure echoing program via `mtl`:

```haskell
import           Availability
import           Availability.State
import           Availability.Writer
import qualified Control.Monad.State                  as MTL
import qualified Control.Monad.Writer                 as MTL
import           Data.Function                        ((&))
import           Data.Maybe                           (fromMaybe)

type PureProgram = MTL.WriterT [String] (MTL.State [String])

makeEffViaMonadWriter [t| "out" |] [t| [String] |] [t| PureProgram |]
makeEffViaMonadState [t| "in" |] [t| [String] |] [t| PureProgram |]

instance Interpret Teletype PureProgram where
  type InTermsOf _ _ = '[Getter "in" [String], Putter "in" [String], Teller "out" [String]]
  interpret = \case
    ReadTTY -> get @"in" >>= \case
      []     -> pure ""
      x : xs -> x <$ put @"in" xs
    WriteTTY msg -> tell @"out" [msg]

echoPure :: Eff Teletype => M PureProgram ()
echoPure = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i >> echoPure

runEchoPure :: [String] -> [String]
runEchoPure s = runUnderlying @'[Teletype] echoPure & MTL.execWriterT & (`MTL.evalState` s)
```

or an impure interpretation directly through `IO`.

```haskell
import           Availability.Embed
import           Availability

makeEffViaMonadIO [t| IO |]

instance Interpret Teletype IO where
  type InTermsOf _ _ = '[Embed IO]
  interpret = \case
    ReadTTY      -> embed getLine
    WriteTTY msg -> embed $ putStrLn msg

echoIO :: Eff Teletype => M IO ()
echoIO = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i >> echoIO

main :: IO ()
main = runUnderlying @'[Teletype] echoIO
```
