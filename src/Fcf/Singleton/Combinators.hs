module Fcf.Singleton.Combinators where

import Fcf.Core
import Fcf.Combinators
import Fcf.Singleton.Core

pureS :: t a -> SExp t (Pure a)
pureS = SExp

pure1S :: (t1 a -> t2 (f a)) -> t1 a -> SExp t2 (Pure1 f a)
pure1S f x = SExp (f x)

pure2S :: (ta a -> tb b -> tc (f a b)) -> ta a -> tb b -> SExp tc (Pure2 f a b)
pure2S f x y = SExp (f x y)

pure3S :: (ta a -> tb b -> tc c -> td (f a b c)) -> ta a -> tb b -> tc c -> SExp td (Pure3 f a b c)
pure3S f x y z = SExp (f x y z)

(&=<<) :: (forall a . t1 a -> SExp t2 (f a)) -> SExp t1 x -> SExp t2 (f =<< x)
f &=<< x = repack . f &- x
infixr 1 &=<<

(&<=<) :: (forall a . t2 a -> SExp t3 (f a)) -> (forall a . t1 a -> SExp t2 (g a)) -> t1 a -> SExp t3 ((f <=< g) a)
f &<=< g = repack . f . eval . g
infixr 1 &<=<

liftM2S :: (forall x y . t1 x -> t2 y -> SExp t3 (f x y)) -> SExp t1 a -> SExp t2 b -> SExp t3 (LiftM2 f a b)
liftM2S f x y = repack (f &- x &- y)

liftM3S :: (forall x y z . t1 x -> t2 y -> t3 z -> SExp t4 (f x y z)) -> SExp t1 a -> SExp t2 b -> SExp t3 c -> SExp t4 (LiftM3 f a b c)
liftM3S f x y z = repack (f &- x &- y &- z)

joinS :: SExp (SExp t) a -> SExp t (Join a)
joinS = repack . eval

(&<$>) :: (forall x . t1 x -> t2 (f x)) -> SExp t1 a -> SExp t2 (f <$> a)
f &<$> x = SExp $ f &- x
infixl 4 &<$>

(&<*>) :: SExp (t1 -&> t2) f -> SExp t1 a -> SExp t2 (f <*> a)
f &<*> x = let (SArr f') = eval f in SExp $ f' &- x
infixl 4 &<*>

flipS :: (forall x y . t1 x -> t2 y -> SExp t3 (f x y)) -> t2 b -> t1 a -> SExp t3 (Flip f b a)
flipS f y x = repack $ f x y

constFnS :: t1 a -> t2 b -> SExp t1 (ConstFn a b)
constFnS = const . SExp

(&$) :: (forall x . t1 x -> SExp t2 (f x)) -> t1 a -> SExp t2 (f $ a)
f &$ x = repack $ f x
infixr 0 &$