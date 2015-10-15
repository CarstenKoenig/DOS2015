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

-- eure Aufgabe ist es jetzt folgende Funktion zu vervollständigen
-- Tipp: ihr solltet den ersten Parameter als "Vorrat" mit noch zu
-- betrachteten Münzen ansehen ... sprich der Wert davon darf sich
-- rekursiv ändern ;)
wechsle :: [Muenze] -> Cent -> [Muenze]
wechsle = undefined

-- damit sollte check "OK" ergeben (probiere es in GHCi!)
check :: String
check = if beispiel == [100,50,2,1] then "OK" else "Sorry"

main :: IO ()
main = do
  putStr "welcher Betrag soll gewechselt werden? "
  betrag <- readLn
  print $ wechsle euroMuenzen betrag
