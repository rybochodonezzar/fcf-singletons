module Fcf.Singleton.Class.Functor where

import Fcf.Core
import Fcf.Class.Functor
import Fcf.Singleton.Core

class FunctorS f where
  fmapS :: (forall x . t1 x -> SExp t2 (r x)) -> f t1 a -> SExp (f t2) (FMap r a)

instance FunctorS SList where
  fmapS f (SCons x xs) = SExp $ SCons &- f x &- fmapS f xs
  fmapS f SNil = SExp SNil

instance FunctorS SMaybe where
  fmapS f (SJust x) = SExp $ SJust &- f x
  fmapS _ SNothing  = SExp SNothing

instance FunctorS (SEither tl) where
  fmapS _ (SLeft x)  = SExp $ SLeft x
  fmapS f (SRight x) = SExp $ SRight &- f x

instance FunctorS (SPair t1) where
  fmapS f (SPair x y) = SExp $ SPair x &- f y