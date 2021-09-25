{-# LANGUAGE DeriveAnyClass #-}
module Error where

import           Availability
import           Availability.Embed
import           Availability.Error
import           Control.Exception        (Exception)
import           Control.Monad.Catch      (MonadThrow)
import           Control.Monad.Catch.Pure (MonadCatch)
import           Control.Monad.IO.Class   (MonadIO)
import           GHC.Generics             (Generic)
import           Test.Common              (shouldPrint, withInput)
import           Test.Hspec               (Spec, describe, it)
import           Text.Read                (readEither)

data ParserError
  = InvalidInput String
  deriving (Show, Exception)

parseNumber :: Sendable (Thrower ParserError) m => String -> M m Int
parseNumber input = case readEither input of
  Left err  -> throwError $ InvalidInput err
  Right num -> pure num
{-# INLINE parseNumber #-}

data MathError
  = NegativeInput
  deriving (Show, Exception)

sqrtNumber :: Sendable (Thrower MathError) m => Int -> M m Int
sqrtNumber num
  | num < 0 = throwError NegativeInput
  | otherwise = pure $ round $ sqrt @Double $ fromIntegral num
{-# INLINE sqrtNumber #-}

-- | Errors that can occur in the calculator application.
data CalcError
    -- | The parser component failed.
  = ParserError ParserError
    -- | The math component failed.
  | MathError MathError
  deriving (Generic, Show, Exception)

newtype Calculator a = Calculator { runCalculator :: IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)
  deriving (Interpret (Embed IO)) via ViaMonadIO Calculator
  deriving (Interpret (Thrower e)) via ViaMonadThrow Calculator
  deriving (Interpret (Catcher e)) via ViaMonadCatch Calculator

type CatchAll = Effs '[Catcher ParserError, Catcher MathError, Catcher CalcError]

calculator :: (CatchAll, Effs '[Embed IO]) => M Calculator ()
calculator = do
  embed $ putStr "Enter positive number or 'Q' to quit\n> "
  line <- embed getLine
  case line of
    "Q" -> pure ()
    input -> do
      catchError @CalcError
        do
          num <- wrapError ParserError $ parseNumber input
          root <- wrapError MathError $ sqrtNumber num
          embed $ putStrLn $ "sqrt = " ++ show root
        \e -> embed $ putStrLn $ "Error: " ++ show e
      calculator

spec :: Spec
spec = do
  describe "Calculator" $
    it "evaluates calculator" $ do
      let
        input = "4\n-1\nxyz\nQ\n"
        output =
          "Enter positive number or 'Q' to quit\n\
          \> sqrt = 2\n\
          \Enter positive number or 'Q' to quit\n\
          \> Error: MathError NegativeInput\n\
          \Enter positive number or 'Q' to quit\n\
          \> Error: ParserError (InvalidInput \"Prelude.read: no parse\")\n\
          \Enter positive number or 'Q' to quit\n\
          \> "
      runCalculator (runM @'[Catcher ParserError, Catcher MathError, Catcher CalcError, Embed IO] calculator)
        `withInput` input
        `shouldPrint` output
