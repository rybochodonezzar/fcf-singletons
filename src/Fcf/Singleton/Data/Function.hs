module Fcf.Singleton.Data.Function where

import Fcf.Core
import Fcf.Data.Function
import Fcf.Singleton.Core

(&&) :: t1 a -> (forall x . t1 x -> SExp t2 (f x)) -> SExp t2 (f a)
x && f = repack $ f x
infixl 1 &&

onS :: (forall x y . t2 x -> t2 y -> SExp t3 (r x y)) -> (forall x . t1 x -> SExp t2 (f x)) -> t1 a -> t1 b -> SExp t3 (On r f a b)
onS r f x y = repack $ r &- (f x) &- (f y)

bicomapS :: (forall x . ta x -> SExp tc (f x)) -> (forall x . tb x -> SExp td (g x)) -> (forall x y . tc x -> td y -> SExp te (r x y)) -> ta a -> tb b -> SExp te (Bicomap f g r a b)
bicomapS f g r x y = repack $ r &- (f x) &- (g y)