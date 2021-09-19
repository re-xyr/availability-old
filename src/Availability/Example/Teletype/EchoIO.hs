{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
module Availability.Example.Teletype.EchoIO where

import           Availability
import           Availability.Embed
import           Availability.Example.Teletype.Effect

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
