{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Fuzzy where
import Control.Monad (liftM)

newtype Weight a = Weight a deriving (Eq, Ord, Num, Enum, Real, Fractional, Show)
newtype Var a = Var a deriving (Eq, Ord, Num, Enum, Real, Fractional, Show)
newtype Set v w = Set (v -> w)

class Logical l where
  (&?) :: l -> l -> l
  (|?) :: l -> l -> l
  (!?) :: l -> l
infix 9 !?
infix 8 |?
infix 8 &?

class Rule w v where
  (==>) :: (Weight w) -> (Set (Var v) (Weight w)) -> (Set (Var v) (Weight w))
infix 6 ==>

class Ring a where
  zero :: a
  unit :: a
  
class VarMonad v a | v -> a where
  var :: v -> a

class WeightMonad w a | w -> a where
  weight :: w -> a

instance WeightMonad (Weight a) a where
  weight (Weight a) = a
  
class Hedge h where
  negation :: h -> h
  very :: h -> h
  fairly :: h -> h
  
instance (Num w, Floating w) => Hedge (Set (Var v) (Weight w)) where
  negation (Set f) = Set $ liftM (1 -) . f
  very (Set f) = Set $ liftM (**2) . f
  fairly (Set f) = Set $ liftM sqrt . f

instance (Real a) => VarMonad (Var a) Rational where
  var (Var x) = toRational x

instance Functor Weight where
  fmap = liftM

instance Applicative Weight where
  pure = return
  f <*> v = do
    f' <- f
    v' <- v
    return $ f' v'

instance Monad Weight where
  return = Weight
  Weight v >>= f = f v

instance Functor Var where
  fmap = liftM

instance Applicative Var where
  pure = return
  f <*> v = do
    f' <- f
    v' <- v
    return $ f' v'

instance Monad Var where
  return = Var
  Var v >>= f = f v
    
instance Ring Double where
  zero = 0.0
  unit = 1.0
  
instance Ring Rational where
  zero = 0.0
  unit = 1.0
  
instance (Ring a) => Ring (Weight a) where
  zero = Weight zero
  unit = Weight unit

instance (Ring a) => Ring (Var a) where
  zero = Var zero
  unit = Var unit

up :: (Ord a, Fractional a, Real a) => Var a -> Var a -> Set (Var a) (Weight Double)
up mn mx = Set bound where
  bound x
    | x <= mn = zero
    | x <= mx = Weight bound'
    | otherwise = unit
    where
      bound' = fromRational . var $ (x - mn) / (mx - mn)

down :: (Ord a, Fractional a, Real a) => Var a -> Var a -> Set (Var a) (Weight Double)
down mn mx = Set bound where
  bound x
    | x <= mn = unit
    | x <= mx = Weight bound'
    | otherwise = zero
    where
      bound' = fromRational . var $ (x - mx) / (mn - mx)

gauss :: () => Var Double -> Var Double -> Set (Var Double) (Weight Double)
gauss (Var mu) (Var sgm) = Set bound where
  bound (Var x) = Weight $ min 1.0 a*exp(-0.5*((x-mu)/sgm)**2) where
    a = 2 / sgm * sqrt(2*pi)

sigmoid :: () => Var Double -> Var Double -> Set (Var Double) (Weight Double)
sigmoid (Var mn) (Var mx) = Set bound where
  bound (Var x) = Weight $ sigmoid'' x where
    sigmoid' a = 1/(1+exp(-1*a))
    sigmoid''=   sigmoid' . (flip (-) (0.5*(mx+mn)))

(=?) :: Var v -> Set (Var v) (Weight w) -> Weight w
(=?) v (Set f)= f v 
infix 7 =?

instance (Ord w, Num w, Ring w) => Logical (Weight w) where
  (&?) (Weight w1) (Weight w2) = Weight $ min w1 w2 
  (|?) (Weight w1) (Weight w2) = Weight $ max w1 w2 
  (!?) (Weight w1) = Weight $ min unit $ max zero $ unit - w1

instance (Ord w, Num w, Ring w) => Logical (Set (Var v) (Weight w)) where
  (&?) (Set f1) (Set f2) = Set $ \x -> min (f1 x) (f2 x)
  (|?) (Set f1) (Set f2) = Set $ \x -> max (f1 x) (f2 x)
  (!?) (Set f) = Set $ \x -> min unit $ max zero $ unit - (f x)

instance (Ord w) => Rule w v where
  (==>) w' (Set f) = Set $ \x -> min w' $ f x

data Centroid v = Centroid { domainMin :: v,
                             domainMax :: v,
                             resolution :: v
                           }

class ClassCentroid c v w where
  crisp :: (c v) -> [Set v w] -> v

instance ClassCentroid Centroid (Var Double) (Weight Double) where
  crisp c xs = Var bound where
    Var bound = (fst elim) / (snd elim)
    elim = foldl func (zero, zero) xs
    func (vnum, vden) (Set f) = (vnum + foldl (fnum) zero domain, vden + foldl (fdenum) zero domain) where
      fnum accumv vari = 
        let
          { Weight w = f vari;
            Var i = vari;
            y = Var $ i * w
          }
        in accumv + y
      fdenum accumv vari = 
        let
          { Weight w = f vari;
            y = Var w
          }
        in accumv + y
    domain = [domainMin c, (+) (domainMin c) (resolution c) .. (domainMax c)]
    
