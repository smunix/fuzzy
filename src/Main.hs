module Main where

import Fuzzy

alertCentroid :: Centroid (Var Double)
alertCentroid = Centroid { domainMin = Var 0,
                           domainMax = Var 100,
                           resolution = Var 0.5
                         }

type Dom = Weight Double

type MS_Notional = Var Double
type MKT_Notional = Var Double
type Alert = Var Double

alert_high, alert_medium, alert_low :: Set Alert Dom
alert_high = sigmoid (Var 75) (Var 100)
alert_medium = gauss (Var 50) (Var 50)
alert_low = negation alert_high

ms_nv_high, ms_nv_medium, ms_nv_low :: Set MS_Notional Dom
ms_nv_high = sigmoid (Var 0) (Var 1000)
ms_nv_medium = gauss (Var 500) (Var 100)
ms_nv_low = negation ms_nv_high

mkt_nv_high, mkt_nv_medium, mkt_nv_low :: Set MKT_Notional Dom
mkt_nv_high = sigmoid (Var 0) (Var 1000)
mkt_nv_medium = gauss (Var 500) (Var 100)
mkt_nv_low = negation mkt_nv_high

rule1, rule2, rule3, rule4, rule5, rule6 :: MS_Notional -> MKT_Notional -> Set Alert Dom
rule1 ms mkt = (ms =? ms_nv_low) &? (mkt =? mkt_nv_low) ==> very alert_low
rule2 ms mkt = (ms =? ms_nv_low) &? (mkt =? mkt_nv_high) ==> very alert_low
rule3 ms mkt = (ms =? ms_nv_high) &? (mkt =? mkt_nv_low) ==> very alert_high

rule4 ms mkt = (ms =? ms_nv_high) &? (mkt =? mkt_nv_medium) ==> alert_high
rule5 ms mkt = (ms =? ms_nv_medium) &? (mkt =? mkt_nv_medium) ==> very alert_low
rule6 ms mkt = (ms =? ms_nv_high) &? (mkt =? mkt_nv_high) ==> very alert_low

rules :: MS_Notional -> MKT_Notional -> [Set Alert Dom]
rules ms mkt = [rule1 ms mkt, rule2 ms mkt, rule3 ms mkt, rule4 ms mkt, rule5 ms mkt, rule6 ms mkt]

exec :: MS_Notional -> MKT_Notional -> IO ()
exec ms mkt =
  let
    Var r = crisp alertCentroid (rules ms mkt)
  in
   do
     putStrLn $ "exec {ms=" ++ (show ms) ++ ", mkt=" ++ (show mkt) ++ "} ==> Alert=" ++ (show r) ++ "%"

main :: IO ()
main = do
  -- exec MS MKT
  
  exec (Var 100) (Var 200)
  exec (Var 100) (Var 400)
  exec (Var 100) (Var 600)
  exec (Var 100) (Var 800)

  exec (Var 100) (Var 200)
  exec (Var 300) (Var 200)
  exec (Var 500) (Var 200)
  exec (Var 700) (Var 200)
  exec (Var 900) (Var 200)
  exec (Var 1000) (Var 200)

  exec (Var 100) (Var 200)
  exec (Var 300) (Var 200)
  exec (Var 500) (Var 400)
  exec (Var 700) (Var 700)
  exec (Var 900) (Var 800)
  exec (Var 1000) (Var 950)
