{-# LANGUAGE TypeFamilies #-}
module Fcf.Singleton.Class.Monoid.Types where

import Fcf.Core
import Fcf.Class.Monoid.Types
import Fcf.Singleton.Core
import Fcf.Combinators

data SEndo :: (a -> *) -> Endo a -> * where
  SEndo :: forall t a f . (forall x . t x -> SExp t (f x)) -> SEndo t ('Endo f)

unEndoS :: SEndo t ('Endo f) -> (forall x . t x -> SExp t (f x))
unEndoS (SEndo f) = f

-- data RunEndo :: (e :: Endo a) -> a -> Exp a
-- type instance Eval (RunEndo f x) = UnEndo f =<< x