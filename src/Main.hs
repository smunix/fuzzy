module Main where
import Control.Monad (liftM)

newtype Weight a = Weight a deriving (Eq, Ord, Num, Enum, Real, Fractional, Show)
newtype Var a = Var a deriving (Eq, Ord, Num, Enum, Real, Fractional, Show)
newtype Set v w = Set { domSet :: v -> w }

class VarMonad v a | v -> a where
  var :: v -> a

class WeightMonad w a | w -> a where
  weight :: w -> a

class Hedge h where
  oppose :: h -> h
  very :: h -> h
  fairly :: h -> h
  
instance (Num w, Floating w) => Hedge (Set (Var v) (Weight w)) where
  oppose Set{ domSet = f } = Set{ domSet = liftM (1 -) . f}
  very Set{ domSet = f } = Set{ domSet = liftM (**2) . f }
  fairly Set{ domSet = f } = Set{ domSet = liftM sqrt . f}

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
    
class Ring a where
  zero :: a
  unit :: a
  
class LingVar a where
  resolution :: a

instance Ring Double where
  zero = 0.0
  unit = 1.0
  
instance Ring Rational where
  zero = 0.0
  unit = 1.0
  
instance (Ring a) => Ring (Weight a) where
  zero = Weight zero
  unit = Weight unit
  
up :: (Ord a, Fractional a, Real a) => Var a -> Var a -> Set (Var a) (Weight Double)
up mn mx = Set { domSet = bound } where
  bound x
    | x <= mn = zero
    | x <= mx = Weight bound'
    | otherwise = unit
    where
      bound' = fromRational . var $ (x - mn) / (mx - mn)

down :: (Ord a, Fractional a, Real a) => Var a -> Var a -> Set (Var a) (Weight Double)
down mn mx = Set { domSet = bound } where
  bound x
    | x <= mn = unit
    | x <= mx = Weight bound'
    | otherwise = zero
    where
      bound' = fromRational . var $ (x - mx) / (mn - mx)

gauss :: () => Var Double -> Var Double -> Set (Var Double) (Weight Double)
gauss (Var mu) (Var sgm) = Set { domSet = bound } where
  bound (Var x) = Weight $ min 1.0 a*exp(-0.5*((x-mu)/sgm)**2) where
    a = 2 / sgm * sqrt(2*pi)

sigmoid :: () => Var Double -> Var Double -> Set (Var Double) (Weight Double)
sigmoid (Var mn) (Var mx) = Set { domSet = bound } where
  bound (Var x) = Weight $ sigmoid'' x where
    sigmoid' a = 1/(1+exp(-1*a))
    sigmoid''=   sigmoid' . (flip (-) (0.5*(mx+mn)))

type Height = Var Double
type Dom = Weight Double

tall, mean, small :: Set Height Dom
tall = sigmoid (return 10) (return 20)
small = oppose tall
mean = gauss (return 15) (return 5)

type MS_Notional = Var Double
type MKT_Notional = Var Double
type Alert = Var Double

alert_high, alert_medium, alert_low :: Set Alert Dom
alert_high = sigmoid (Var 50) (Var 100)
alert_medium = gauss (Var 50) (Var 25)
alert_low = oppose alert_high

ms_nv_high, ms_nv_medium, ms_nv_low :: Set MS_Notional Dom
ms_nv_high = sigmoid (Var 0) (Var 1000)
ms_nv_medium = gauss (Var 500) (Var 100)
ms_nv_low = oppose ms_nv_high

mkt_nv_high, mkt_nv_medium, mkt_nv_low :: Set MKT_Notional Dom
mkt_nv_high = sigmoid (Var 0) (Var 1000)
mkt_nv_medium = gauss (Var 500) (Var 100)
mkt_nv_low = oppose mkt_nv_high

(=?) :: Var v -> Set (Var v) (Weight w) -> Weight w
(=?) v (Set{ domSet = dom})= dom v 

class Logical l where
  (&?) :: l -> l -> l
  (|?) :: l -> l -> l

instance (Ord w) => Logical (Weight w) where
  (&?) (Weight w1) (Weight w2) = Weight $ min w1 w2 
  (|?) (Weight w1) (Weight w2) = Weight $ max w1 w2 
  
{-
rule1, rule2, rule3, rule4, rule5, rule6 :: MS_Notional -> MKT_Notional -> Set Alert Dom
rule1 ms mkt = ms =? ms_nv_low &? mkt =? mkt_nv_low ==> low_alert
rule2 ms mkt = ms =? ms_nv_low &? mkt =? mkt_nv_high ==> low_alert
rule3 ms mkt = ms =? ms_nv_high &? mkt =? mkt_nv_low ==> high_alert

rule4 ms mkt = ms =? ms_nv_low &? mkt =? mkt_nv_low ==> low_alert
rule5 ms mkt = ms =? ms_nv_low &? mkt =? mkt_nv_high ==> low_alert
rule6 ms mkt = ms =? ms_nv_high &? mkt =? mkt_nv_low ==> high_alert

-}

main :: IO ()
main = putStrLn "hello"
