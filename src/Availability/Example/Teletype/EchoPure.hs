{-# OPTIONS_GHC -Wno-orphans #-}
module Availability.Example.Teletype.EchoPure where

import           Availability.Example.Teletype.Effect
import           Availability.Impl
import           Availability.State
import           Availability.Writer
import qualified Control.Monad.State                  as MTL
import qualified Control.Monad.Writer                 as MTL
import           Data.Function                        ((&))

type PureProgram = MTL.WriterT [String] (MTL.State [String])

makeEffViaMonadWriter [t| "out" |] [t| [String] |] [t| PureProgram |]
makeEffViaMonadState [t| "in" |] [t| [String] |] [t| PureProgram |]

instance Interpret Teletype PureProgram where
  type InTermsOf _ _ = '[Getter "in" [String], Putter "in" [String], Teller "out" [String]]
  unsafeSend = \case
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
-- >>> runEchoPure ["abc", "def", "ghci"]
-- ["abc","def","ghci"]
