module Fcf.Singleton.Core where

import Fcf.Core
import Data.Void
import Data.Proxy
import GHC.TypeLits

newtype SExp t a = SExp { eval :: t (Eval a) }

repack :: Eval a ~ Eval b => SExp t a -> SExp t b
repack = SExp . eval

(&-) :: (t (Eval a) -> b) -> SExp t a -> b
f &- sx = f $ eval sx
infixl 5 &-

newtype t1 -&> t2 :: (a -> b) -> * where
  SArr :: (forall x . t1 x -> t2 (f x)) -> (t1 -&> t2) f

class RepWithT (f :: k' -> *) k (t :: k -> *) | f k -> t
instance RepWithT (t :: k -> *) k t

class RepWithT f k t => RepWith f t (a :: k) | f k -> t where
  repWith' :: Proxy f -> t a

class RepAs t a where
  repAs :: t a

-- apply custom singleton
instance RepAs t a => RepWith t t a where
  repWith' _ = repAs @t @a

-- any canonical singleton is valid non-canonical singleton
instance RepWith SVoid t a => RepAs t a where
  repAs = repWith' (Proxy @SVoid)

repWith :: forall f a t . RepWith f t a => t a
repWith = repWith' (Proxy @f)

data SVoid :: Void -> * where
  SAbsurd :: SVoid v

instance RepWithT f Void SVoid
instance RepWith f SVoid v where
  repWith' _ = SAbsurd

class RepWith SVoid t a => Rep t a | a -> t where
  rep' :: t a

instance RepWith SVoid t a => Rep t a where
  rep' = repWith' (Proxy @SVoid)

rep :: forall a t . Rep t a => t a
rep = rep' @t @a

data SBool :: Bool -> * where
  SFalse :: SBool 'False
  STrue  :: SBool 'True

deriving instance Show (SBool b)

instance RepWithT f Bool SBool
instance RepWith  f SBool 'False where repWith' _ = SFalse
instance RepWith  f SBool 'True  where repWith' _ = STrue

data SSym s where
  SSym :: KnownSymbol s => SSym s

instance RepWithT f Symbol SSym
instance KnownSymbol k => RepWith f SSym k where repWith' _ = SSym
instance Show (SSym s) where show s@SSym = symbolVal s 

data SNat n where
  SNat :: KnownNat n => SNat n

instance RepWithT f Nat SNat
instance KnownNat n => RepWith f SNat n where repWith' _ = SNat
instance Show (SNat n) where show n@SNat = show $ natVal n

data SPair :: (a -> *) -> (b -> *) -> (a,b) -> * where
  SPair :: t1 a -> t2 b -> SPair t1 t2 '(a,b)

instance (forall a . Show (t1 a), forall b . Show (t2 b)) => Show (SPair t1 t2 '(a,b)) where
  show (SPair t1 t2) = "'(" ++ show t1 ++ ", " ++ show t2 ++ ")"

instance (RepWithT f k1 t1, RepWithT f k2 t2) => RepWithT f (k1, k2) (SPair t1 t2) 
instance (RepWith f t1 a, RepWith f t2 b) => RepWith f (SPair t1 t2) '(a,b) where
  repWith' p = SPair (repWith' p) (repWith' p)

data SList t xs where
  SNil  :: SList t '[]
  SCons :: t x -> SList t xs -> SList t (x ': xs)

instance RepWithT f k t => RepWithT f [k] (SList t)
instance RepWithT f k t => RepWith f (SList t) ('[] :: [k]) where repWith' _ = SNil
instance (RepWith f t x, RepWith f (SList t) xs) => RepWith f (SList t) (x ': xs) where
  repWith' p = SCons (repWith' p) (repWith' p)

toListWith :: (forall a . t a -> x) -> SList t xs -> [x]
toListWith f SNil = []
toListWith f (SCons t xs) = f t : toListWith f xs

instance (forall a . Show (t a)) => Show (SList t xs) where
  show xs = "'" ++ show (toListWith show xs)

data SMaybe :: (a -> *) -> Maybe a -> * where
  SJust    :: t a -> SMaybe t ('Just a)
  SNothing :: SMaybe t 'Nothing

instance RepWithT f k t => RepWithT f (Maybe k) (SMaybe t)
instance RepWithT f k t => RepWith f (SMaybe t) ('Nothing :: Maybe k) where repWith' _ = SNothing
instance RepWith  f t x => RepWith f (SMaybe t) ('Just x) where repWith' p = SJust $ repWith' p

data SEither :: (l -> *) -> (r -> *) -> (Either l r) -> * where
  SLeft  :: tl l -> SEither tl tr ('Left l)
  SRight :: tr r -> SEither tl tr ('Right r)

instance (RepWithT f l kl, RepWithT f r kr) => RepWithT f (Either l r) (SEither kl kr)
-- instance RepWithT f (Either l r) (SEither kl kr) => RepWith f (SEither kl kr) ('Left x) where repWith' p = SLeft $ repWith' p
instance (RepWithT f (Either l r) (SEither kl kr), RepWith f kl x) => RepWith f (SEither kl kr) ('Left x)  where repWith' p = SLeft  $ repWith' p
instance (RepWithT f (Either l r) (SEither kl kr), RepWith f kr x) => RepWith f (SEither kl kr) ('Right x) where repWith' p = SRight $ repWith' p