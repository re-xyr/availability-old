module Availability.Fresh (Fresh, fresh, makeFreshByState, Iter, iter, makeIterByState) where

import           Availability.Getter
import           Availability.Impl
import           Availability.Putter
import           Language.Haskell.TH (Dec, Q, Type)

type Fresh tag = Getter tag Int

fresh :: forall tag m. Sendable (Fresh tag) m => M m Int
fresh = get @tag
{-# INLINE fresh #-}

makeFreshByState :: Q Type -> Q Type -> Q Type -> Q [Dec]
makeFreshByState tag otag mnd =
  [d|
  instance Interpret (Fresh $tag) $mnd where
    type InTermsOf (Fresh $tag) $mnd = '[Getter $otag Int, Putter $otag Int]
    {-# INLINABLE unsafeSend #-}
    unsafeSend Get = do
      x <- get @($otag)
      put @($otag) (x + 1)
      pure x
  |]

type Iter tag s = Getter tag (Maybe s)

iter :: forall tag s m. Sendable (Iter tag s) m => M m (Maybe s)
iter = get @tag
{-# INLINE iter #-}

makeIterByState :: Q Type -> Q Type -> Q Type -> Q Type -> Q [Dec]
makeIterByState tag typ otag mnd =
  [d|
  instance Interpret (Iter $tag $typ) $mnd where
    type InTermsOf (Iter $tag $typ) $mnd = '[Getter $otag [$typ], Putter $otag [$typ]]
    {-# INLINABLE unsafeSend #-}
    unsafeSend Get = do
      x <- get @($otag)
      case x of
        []     -> pure Nothing
        y : ys -> do
          put @($otag) ys
          pure (Just y)
  |]
