{-# LANGUAGE DefaultSignatures #-}
module Fcf.Singleton.Class.Foldable where

import Fcf.Core
import Fcf.Class.Foldable
import Fcf.Singleton.Core
import Fcf.Singleton.Class.Monoid
import Fcf.Singleton.Class.Monoid.Types
import Fcf.Singleton.Combinators

-- Eval (UnEndo (Eval (FoldMap (Pure1 'Endo <=< Pure1 f) xs)) y)

class FoldableS f where
  foldrS         :: (forall x y . ta x -> tb y -> SExp tb (p x y)) -> tb b -> f ta as -> SExp (f tb) (Foldr p b a)
  -- default foldrS :: (forall x y . ta x -> tb y -> SExp tb (p x y)) -> tb b -> f ta as -> SExp (f tb) (Foldr p b a)
  -- foldrS f y xs = unEndoS  
    --repack (unEndo &=<< (foldMapS (pure1S SEndo &<=< pure1S f) xs) y)
  foldMapS :: MonoidS m => (forall x . t x -> SExp m (p x)) -> f m as -> SExp m (FoldMap p as)
  -- foldMapS f xs = 
