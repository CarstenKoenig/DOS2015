----------------------------------------------------------------
-- Jedes Modul beginnt mit diesen Teil
-- weil wir ein lauffähiges Programm haben wollen
-- schreiben wir nur das Hauptmodul (`Main`)

module Main where

-- danach kommt de Teil wo externe Module improtiert werden

import Control.Monad (forM_)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

----------------------------------------------------------------
-- dieser Teil ist der Programmeinstiegspunkt
-- `main` fragt einfach nur die Zahlen und die Zielzahl ab
-- (die Zahlen können als Liste wie in Haskell eingegeben werden)
-- berechnet dann die Lösungen und gibt diese aus
-- die Details sollen heute nicht so interessant sein

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Countdown"
  putStr "Liste mit verfügbaren Zahlen? "
  zahlen <- read <$> getLine
  putStr "Zielzahl? "
  ziel <- read <$> getLine
  let lösungen = bruteForceSolutions zahlen ziel
  forM_ lösungen print

---------------------------------------------------------------
-- HIER beginnt der Spaß für uns ;)

-- unser ziel ist es eine Expression aufzubauen
-- deren Value Blätter nur Werte aus den gegebenen
-- Zahlen annehmen und die dem Zielwert entspricht
data Expression
  = Value Int
  | Apply Operand Expression Expression
  deriving (Eq)

-- dafür stehen uns folgende Operationen zur Auswahl:
data Operand
  = Add | Sub | Mul | Div
  deriving (Eq, Ord)

-- aber nicht Operationen sind für alle Zahlen erlaubt!
-- isValidOp soll True ergeben, wenn alles in Ordnung ist,
-- aber False, falls wir nicht-positive Zahlen produzieren
-- würden (z.B. 4-6), durch 0 teilen oder beim Teilen einen
-- Rest lassen würden (7/4 - 8/4 ist ok)
isValidOp :: (Ord a, Integral a) => Operand -> a -> a -> Bool
isValidOp = undefined

-- apply soll zwei Zahlen mit den gegebenen Operanden
-- verknüpfen und das Ergebnis der Operation berechnen
apply :: Operand -> Int -> Int -> Int
apply = undefined

-- wie oben erwähnt soll eine gültige Expression
-- nur Zahlen aus der gegebenen Liste enthalten
-- mit values suchen wir alle verwendeten Werte
-- (die Werte der Blätter des Expression-Baums)
values :: Expression -> [Int]
values = undefined

-- um später alle möglichen Expressions aufzulisten
-- brauchen wir alle Teillisten der Verfügbaren
-- Zahlen (wobei hier die Anordnung bestehen bleiben soll)
subs :: [a] -> [[a]]
subs = undefined

-- um gleich perms zu implementieren können wir
-- pick benutzen: es liefert alle Möglichkeiten
-- ein Element aus einer Liste zu wählen (zusammen mit
-- den Rest)
-- Beispiel: pick [1,2,3] = [(1,[2,3]),(2,[1,3]),(3,[1,2])]
pick :: [a] -> [ (a,[a]) ]
pick = undefined

-- schließlich brauchen wir noch eine Funktion die uns
-- alle Permutationen (Umordnungen) einer Liste liefert
-- Tipp: Pick ist sicher nützlich ;)
perms :: [a] -> [[a]]
perms = undefined

-- wir haben das oben noch nicht gemacht, aber wir müssen
-- eine Expression ja noch "auswerten"
-- allerdings benutzen wir hier einen Trick: eval soll nicht
-- nur ein Ergebnis liefern - es soll auch prüfen ob überall
-- valide Operationen verwendet wurden, und dafür können wir
-- Listen und Comprehensions clever benutzen:
-- die Leere Menge steht für kein Ergebnis möglich (z.B. weil
-- irgendwo durch 0 geteilt wurde) und ein Ergebnis n wird
-- durch eine Liste mit genau einem Element [n] angezeigt
eval :: Expression -> [Int]
eval = undefined

-- subbags verbindet jetzt einfach perms und subs: damit bekommen
-- wir alle Permutationen aller Teillisten:
subbags :: [a] -> [[a]]
subbags = undefined

-- diese Funktion liefert alle Möglichkeiten wie
-- eine Liste in zwei Teile geteilt werden kann
split :: [a] -> [ ([a],[a]) ]
split = undefined

-- allerdings interessieren wir uns nur für die Aufteilungen
-- wo kein Teil leer ist
notEmptySplit :: [a] -> [ ([a],[a]) ]
notEmptySplit = filter notEmpty . split
  where notEmpty (xs,ys) = not (null xs || null ys)

-- wie wollen Expressions erzeugen und wir machen das, indem
-- wir eine Menge von Zahlen in jeder möglichen Weise in zwei
-- Teile teilen und dann rekursiv für diese Teile Expressions
-- erzeugen - die rekursiven Teile ergeben dann wieder 4
-- weitere Möglichkeiten - eine für jeden Operator, mit den
-- wir die beiden Unterexpressions verbinden können.
-- hatte die Liste nur ein Element kann es nur
-- ein Blatt (eine Value werden) und bei keinem Element ist
-- es offensichtlich unmöglich eine Expression zu erzeugen
expressions :: [Int] -> [Expression]
expressions []  = []
expressions [n] = [Value n]
expressions ns  = [ Apply op l r | (ls,rs) <- notEmptySplit ns
                                 , l <- expressions ls
                                 , r <- expressions rs
                                 , op <- [Add, Sub, Mul, Div] ]

-- damit können wir jetzt per Brute-Force alle Lösungen suchen:
-- für jede Teilliste und Permutation dieser (subbags) suchen
-- wir in jeder mit dieser Teilzahlenliste bildbaren Expression
-- genau die heraus, deren Auswertung n ergibt:
bruteForceSolutions :: [Int] -> Int -> [Expression]
bruteForceSolutions ns n = [ e | ns' <- subbags ns, e <- expressions ns', eval e == [n]]

------------------------------------------------------
-- dieser Teil dient nur dazu die Expressions etwas
-- shöner Darzustellen, als es `deriving Show` könnte

instance Show Expression where
  show ex = snd $ formatEx 0 ex

formatEx :: Int -> Expression -> (Int, String)
formatEx _ (Value n) = (9, show n)
formatEx prec (Apply op l r)
  | opPrec <= prec = (prec, "(" ++ formatted ++ ")")
  | otherwise     = (prec, formatted)
  where opPrec    = precedence op
        formatted = let (lp, ls) = formatEx opPrec l
                        (_,  rs) = formatEx lp r
                    in  ls ++ show op ++ rs

precedence :: Operand -> Int
precedence Mul = 9
precedence Div = 8
precedence Add = 5
precedence Sub = 4

instance Show Operand where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
