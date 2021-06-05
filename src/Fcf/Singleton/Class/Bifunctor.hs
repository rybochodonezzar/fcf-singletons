module Fcf.Singleton.Class.Bifunctor where

import Fcf.Core
import Fcf.Class.Bifunctor
import Fcf.Singleton.Core

class BifunctorS f where
  bimapS  :: (forall x . tl x -> SExp tl' (p x)) -> (forall x . tr x -> SExp tr' (q x)) -> f tl tr lr -> SExp (f tl' tr') (Bimap p q lr)
  firstS  :: (forall x . tl x -> SExp tl' (p x)) -> f tl tr lr -> SExp (f tl' tr) (First  p lr)
  secondS :: (forall x . tr x -> SExp tr' (p x)) -> f tl tr lr -> SExp (f tl tr') (Second p lr)

instance BifunctorS SEither where
  bimapS f _ (SLeft x)  = SExp $ SLeft  &- f x
  bimapS _ g (SRight x) = SExp $ SRight &- g x
  firstS f (SLeft x)    = SExp $ SLeft &- f x
  firstS _ (SRight x)   = SExp $ SRight x
  secondS _ (SLeft x)   = SExp $ SLeft x
  secondS f (SRight x)  = SExp $ SRight &- f x

instance BifunctorS SPair where
  bimapS f g (SPair x y) = SExp $ SPair &- f x &- g y
  firstS f (SPair x y)   = SExp $ SPair &- f x $ y
  secondS f (SPair x y)  = SExp $ SPair x &- f y