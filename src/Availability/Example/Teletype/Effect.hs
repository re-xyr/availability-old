{-# OPTIONS_HADDOCK hide #-}
module Availability.Example.Teletype.Effect where

import           Availability (Effect, M, Sendable, send)

data Teletype :: Effect where
  ReadTTY :: Teletype m String
  WriteTTY :: String -> Teletype m ()

readTTY :: Sendable Teletype m => M m String
readTTY = send ReadTTY

writeTTY :: Sendable Teletype m => String -> M m ()
writeTTY s = send (WriteTTY s)
