# availability

`availability` is an unconventional effects library.

- Effects are small and lawless.
- Effectful functions use a user-provided fixed monad instead of a universal `Eff` or a variable monad type.

`availability` has good performance and performed better than `fused-effects`, `freer-simple` and `polysemy` in `effect-zoo` microbenchmarks.

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
import           Availability.Fresh
import           Availability.Getter
import           Availability.Impl
import           Availability.MTL.TH
import           Availability.Putter
import qualified Control.Monad.State           as MTL
import qualified Control.Monad.Writer          as MTL
import           Data.Function                 ((&))
import           Data.Maybe                    (fromMaybe)

type PureProgram = MTL.WriterT [String] (MTL.State [String])

makeEffViaMonadWriter [t| "out" |] [t| [String] |] [t| PureProgram |]
makeEffViaMonadState [t| "inImpl" |] [t| [String] |] [t| PureProgram |]
makeIterByState [t| "in" |] [t| String |] [t| "inImpl" |] [t| PureProgram |]

instance Interpret Teletype PureProgram where
  type InTermsOf _ _ = '[Getter "in" (Maybe String), Putter "out" [String]]
  unsafeSend = \case
    ReadTTY      -> fromMaybe "" <$> get @"in"
    WriteTTY msg -> put @"out" [msg]

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
import           Availability.Impl
import           Availability.MTL.TH

makeEffViaMonadIO [t| IO |]

instance Interpret Teletype IO where
  type InTermsOf _ _ = '[Embed IO]
  unsafeSend = \case
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
