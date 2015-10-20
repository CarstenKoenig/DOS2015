module Main where

type Cent   = Int
type Muenze = Cent

-- Ziel dieser Übung ist es einen "Geldwechsler" zu implementieren
-- Auf gegebenen Betrag (in Cent) soll eine minimale Liste mit
-- Münzwerten ausgegeben werden, deren Summe genau den gegebenen
-- Betrag entspricht
--
-- Beispiel:
beispiel :: [Muenze]
beispiel = wechsle euroMuenzen 153 -- sollte [100,50,2,1] ergeben!
--
-- Wir benutzen hier den einfachen Fall mit den uns bekannten Münzen:
euroMuenzen :: [Muenze]
euroMuenzen = [200, 100, 50, 25, 10, 5, 2, 1]

-- der Trick hier ist, die Münzen (die schon sortiert sein sollten)
-- durchzugehen und immer zu schauen, ob der Wert der Münze noch in
-- den Restbetrag "hineinpasst" - abhängig davon wird dann rekursiv
-- nach einem eventuell geänderten Restbetrag mit eventuell geänderten
-- Münzen gesucht:
wechsle :: [Muenze] -> Cent -> [Muenze]
-- ist der Restbetrag 0 sind wir fertig
wechsle _ 0 = []
-- sonst gibt es hoffentlich noch Münzen
wechsle (m:ms) b
  -- die Münze passt noch in den Restbetrag b
  -- d.h. die Ausgabe muss die Münze enthalten (`m :`)
  -- und ein neuer Betrag (b-m) ist zu wechseln
  -- dabei kann ja die Münze m nochmal verwendet
  -- werden (z.B. 2Eur in 4Eur Restbetrag)
  | m <= b     = m : wechsle (m:ms) (b-m)
  -- die Münze passt nicht mehr
  -- dann muss der gleiche Restbetrag aber mit weniger
  -- Münzen gewechselt werden
  | otherwise = wechsle ms b
-- gibt es keine Münzen mehr aber noch einen Restbetrag, haben wir ein Problem
wechsle [] b = error ("keine Münzen mehr übrig um " ++ show b ++ " zu wechseln")

-- damit sollte check "OK" ergeben (probiere es in GHCi!)
check :: String
check = if beispiel == [100,50,2,1] then "OK" else "Sorry"

main :: IO ()
main = do
  putStr "welcher Betrag soll gewechselt werden? "
  betrag <- readLn
  print $ wechsle euroMuenzen betrag
