{-# OPTIONS_GHC -Wno-orphans #-}
module Availability.Example.Teletype.EchoPure where

import           Availability.Example.Teletype.Effect
import           Availability.Fresh
import           Availability.Getter
import           Availability.Impl
import           Availability.MTL.TH
import           Availability.Putter
import qualified Control.Monad.State                  as MTL
import qualified Control.Monad.Writer                 as MTL
import           Data.Function                        ((&))
import           Data.Maybe                           (fromMaybe)

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
-- >>> runEchoPure ["abc", "def", "ghci"]
-- ["abc","def","ghci"]
