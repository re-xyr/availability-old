module Availability.Fresh (Fresh, fresh, makeFreshByState) where

import           Availability
import           Availability.State
import           Language.Haskell.TH (Dec, Q, Type)

data Fresh :: Effect where
  Fresh :: Fresh m Int

fresh :: forall m. Sendable Fresh m => M m Int
fresh = send Fresh
{-# INLINE fresh #-}

makeFreshByState :: Q Type -> Q Type -> Q [Dec]
makeFreshByState otag mnd =
  [d|
  instance Interpret Fresh $mnd where
    type InTermsOf _ _ = '[Getter $otag Int, Putter $otag Int]
    {-# INLINABLE interpret #-}
    interpret Fresh = do
      x <- get @($otag)
      put @($otag) (x + 1)
      pure x
  |]
