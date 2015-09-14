# fuzzy
Fuzzy rule based system for anomalies detection

## Starting this project
cabal init -n -l BSD3 --is-executable --language=Haskell2010 -u smunix.com -a 'Providence Salumu' -c Numeric -s 'Writing fuzzy rules based system' -p fuzzy

A toy fuzzy rule based system I wrote to play with anomalies detections on exchanges.
Below are steps to run it:

## configure
╭─smunix at smunix in ~/Programming/haskell/fuzzy on master✘✘✘ using ‹› 15-09-14 - 2:51:31
╰─○ cabal configure
cabal configure
Resolving dependencies...
Configuring fuzzy-0.1.0.0...

## Build
╭─smunix at smunix in ~/Programming/haskell/fuzzy on master✘✘✘ using ‹› 15-09-14 - 2:52:36
╰─○ cabal build
cabal build
Building fuzzy-0.1.0.0...
Preprocessing library fuzzy-0.1.0.0...
[1 of 1] Compiling Fuzzy            ( src/Fuzzy.hs, dist/build/Fuzzy.o )
In-place registering fuzzy-0.1.0.0...
Preprocessing executable 'fuzzy' for fuzzy-0.1.0.0...
[1 of 2] Compiling Fuzzy            ( src/Fuzzy.hs, dist/build/fuzzy/fuzzy-tmp/Fuzzy.o )
[2 of 2] Compiling Main             ( src/Main.hs, dist/build/fuzzy/fuzzy-tmp/Main.o )
Linking dist/build/fuzzy/fuzzy ...

## Run examples
In the output below, you can see different alert levels we get for different notional values on both the exchanges and the firm.
╭─smunix at smunix in ~/Programming/haskell/fuzzy on master✘✘✘ using ‹› 15-09-14 - 2:52:40
╰─○ dist/build/fuzzy/fuzzy
dist/build/fuzzy/fuzzy
exec {ms=Var 100.0, mkt=Var 200.0} ==> Alert=43.13838185139064%
exec {ms=Var 100.0, mkt=Var 400.0} ==> Alert=43.13838185139064%
exec {ms=Var 100.0, mkt=Var 600.0} ==> Alert=43.13838185139064%
exec {ms=Var 100.0, mkt=Var 800.0} ==> Alert=43.13838185139064%
exec {ms=Var 100.0, mkt=Var 200.0} ==> Alert=43.13838185139064%
exec {ms=Var 300.0, mkt=Var 200.0} ==> Alert=43.13984905285267%
exec {ms=Var 500.0, mkt=Var 200.0} ==> Alert=50.123233797386874%
exec {ms=Var 700.0, mkt=Var 200.0} ==> Alert=94.06152030139934%
exec {ms=Var 900.0, mkt=Var 200.0} ==> Alert=94.2655776014311%
exec {ms=Var 1000.0, mkt=Var 200.0} ==> Alert=94.27189264293638%
exec {ms=Var 100.0, mkt=Var 200.0} ==> Alert=43.13838185139064%
exec {ms=Var 300.0, mkt=Var 200.0} ==> Alert=43.13984905285267%
exec {ms=Var 500.0, mkt=Var 400.0} ==> Alert=50.247414366516935%
exec {ms=Var 700.0, mkt=Var 700.0} ==> Alert=43.221484622722556%
exec {ms=Var 900.0, mkt=Var 800.0} ==> Alert=43.1447221653757%
exec {ms=Var 1000.0, mkt=Var 950.0} ==> Alert=43.1383462743783%

╭─smunix at smunix in ~/Programming/haskell/fuzzy on master✘✘✘ using ‹› 15-09-14 - 2:52:58
╰─○ 
You can define as many rules for alerts as you see fit, and fix the tresholds and resolution as you like.
rule1 ms mkt = (ms =? ms_nv_low) &? (mkt =? mkt_nv_low) ==> very alert_low
rule2 ms mkt = (ms =? ms_nv_low) &? (mkt =? mkt_nv_high) ==> very alert_low
rule3 ms mkt = (ms =? ms_nv_high) &? (mkt =? mkt_nv_low) ==> very alert_high
rule4 ms mkt = (ms =? ms_nv_high) &? (mkt =? mkt_nv_medium) ==> alert_high
rule5 ms mkt = (ms =? ms_nv_medium) &? (mkt =? mkt_nv_medium) ==> very alert_low
rule6 ms mkt = (ms =? ms_nv_high) &? (mkt =? mkt_nv_high) ==> very alert_low

The only deffuzifier I have implememted by now is a Centroid.
alertCentroid :: Centroid (Var Double)
alertCentroid = Centroid { domainMin = Var 0,
                           domainMax = Var 100,
                           resolution = Var 0.5
                         }

Here is how I have defined the domain space:
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

Several functions are readily available for you to define linguistic variables in the domain space:
- up : left shoulder function
- down : right shoulder function
- sigmoid
- gauss : hat like /gaussian function
- triangle (todo)

