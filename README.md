# availability

`availability` is an unconventional effects library. It lets user provide a fixed concrete monad for all effectful functions in an application, and attach effects to the monads in an easy way.

- It is very lightweight ([~60 sloc core](https://github.com/re-xyr/availability/blob/master/src/Availability/Internal/Availability.hs)), easy to understand and [fast](#performance).
- It works with existing ecosystem, like [`mtl`](https://hackage.haskell.org/package/mtl) and [`lens`](https://hackage.haskell.org/package/lens).
- You can use the expressive [`ReaderT` pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern) conveniently.
- You can derive effects from each other via predefined TH functions, e.g. it takes one line to derive `Getter a, Setter a` from `Getter (IORef a)`.

Current effects libraries has one principle of effect restriction: an effect can be used in a monad if it can be interpreted in terms of the monad. This works well with a polymorphic monad type, but a polymorphic type is unfriendly to compiler optimization. In contrast, a concrete monad can be easily optimized, but if fix a monad that supplies all effects we need, we can no longer restrict what effects each function can use.

`availability` solves this problem with the [*phantom constraint pattern*](https://xn--i2r.xn--rhqv96g/2021/09/14/redundant-constraints/). We use a newtype wrapper `M` to screen out the user from directly manipulating the underlying monad, and let user to implement interpretations of effects in terms of other more primitive effects. In any function, the user can use an effect only if:

- The effect is implemented for the monad, and
- The effect constraint `Eff e` is available in the context.

The second requirement decouples the availability of effects from the monad implementation. At last, we use a function `runUnderlying` to clear the constraints and restore the underlying monad. A typical example looks like this:

```haskell
data Ctx = Ctx { _foo :: Int, _bar :: IORef Bool }
makeLenses ''Ctx

type App = ReaderT Ctx IO
makeEffViaMonadIO     [t|App|]
makeEffViaMonadReader [t|"ctx"   |] [t|Ctx       |] [t|App|]
makeReaderFromLens    [t|"foo"   |] [t|Int       |] [t|"ctx"   |] [t|Ctx|] [|foo|] [t|App|]
makeReaderFromLens    [t|"barRef"|] [t|IORef Bool|] [t|"ctx"   |] [t|Ctx|] [|bar|] [t|App|]
makeStateByIORef      [t|"bar"   |] [t|Bool      |] [t|"barRef"|] [t|App|]

testParity :: (Effs '[Getter "foo" Int, Putter "bar" Bool]) => M App ()
testParity = do
  num <- get @"foo" @Int
  put @"bar" (even num)

example :: IO ()
example = do
    rEven <- newIORef False
    runUnderlying @'[Getter "foo" Int, Putter "bar" Bool] testParity
      & (`runReaderT` Ctx 2 rEven)
    readIORef rEven >>= print
    runUnderlying @'[Getter "foo" Int, Putter "bar" Bool] testParity
      & (`runReaderT` Ctx 3 rEven)
    readIORef rEven >>= print
```

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

makeEffViaMonadWriter [t|"out"|] [t|[String]|] [t|PureProgram|]
makeEffViaMonadState  [t|"in" |] [t|[String]|] [t|PureProgram|]

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

makeEffViaMonadIO [t|IO|]

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

## Limitations

- Running effects:
  Because effects in `availability` are detached from the monad structure, they cannot be run on a one-by-one basis. Practically, one can only run all effects and obtain the underlying concrete monad at once via `runUnderlying`. This means there is no exact equivalent to `runReaderT`, `runExceptT` etc on the `M` monad.

  If your application can entirely run on a single transformer stack (in particular, `ReaderT IO`), this is a non-issue because there will be no need to run effects one-by-one. For some other scenarios, there are some solutions that may be used solve this issue:

  - `local` is an almost identical substitute to `runReaderT` without destructing the monad.
  - Similarly, `tryError` is a substitute to `runExceptT`.
  - To simulate `runStateT`, simply set the value before the action and get the value after it.
  - `listen` is a very close analog to `runWriterT`.

  The same problem is present in Tweag's [`capability`](https://hackage.haskell.org/package/capability), whose implementation is in some aspects similar to this library.

- Orphan instances: 
  Directly deriving instances on type aliases (like `type App = ReaderT Global IO`) via TH functions will lead to a warning of orphan instances. This is generally not a problem within `availability` because all defined instances are only expected to be used in current project. There are two choices: Either add the GHC option `-Wno-orphans` to the module using TH functions or use a `newtype` to wrap the transformer stack (you may need to derive `MonadIO`, `MonadReader` etc via `GeneralizedNewtypeDeriving`).
