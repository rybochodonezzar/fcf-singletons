module Fcf.Singleton.Class.Monoid where

import Fcf.Core
import Fcf.Class.Monoid
import Fcf.Singleton.Core
import Fcf.Singleton.Combinators
import Fcf.Singleton.Class.Monoid.Types
import GHC.TypeLits
import Data.Proxy
import Unsafe.Coerce

class MonoidS t where
  memptyS :: t MEmpty
  (&<>) :: t a -> t b -> SExp t (a .<> b)

instance MonoidS SOrd where
  memptyS =  SEQ
  SLT &<> _ = SExp SLT
  SGT &<> _ = SExp SGT
  SEQ &<> a = SExp a

instance MonoidS SSym where
  memptyS = SSym @""
  a@SSym &<> b@SSym = case someSymbolVal $ symbolVal a ++ symbolVal b of
    SomeSymbol (p :: Proxy ab) -> SExp . unsafeCoerce $ SSym @ab

instance MonoidS t => MonoidS (SMaybe t) where
  memptyS = SNothing
  SNothing &<> a = SExp a
  a &<> SNothing = SExp a
  SJust a &<> SJust b = case a &<> b of
    SExp x -> SExp $ SJust x

instance MonoidS (SEndo t) where
  memptyS = SEndo pureS
  (SEndo f) &<> (SEndo g) = SExp (SEndo (f &<=< g))

instance MonoidS (SList t) where
  memptyS = SNil 
  SNil &<> ys = SExp ys
  (SCons x xs) &<> ys = SExp (SCons x (eval (xs &<> ys)))

instance (MonoidS t1, MonoidS t2) => MonoidS (SPair t1 t2) where
  memptyS = SPair memptyS memptyS
  (SPair x1 x2) &<> (SPair y1 y2) = SExp (SPair (eval (x1 &<> y1)) (eval (x2 &<> y2)))