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
  let lösungen = solutions zahlen ziel
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

-- apply soll zwei Zahlen mit den gegebenen Operanden
-- verknüpfen und das Ergebnis der Operation berechnen
apply :: Operand -> Int -> Int -> Int
apply Add = (+)
apply Sub = (-)
apply Mul = (*)
apply Div = div

-- wie oben erwähnt soll eine gültige Expression
-- nur Zahlen aus der gegebenen Liste enthalten
-- mit values suchen wir alle verwendeten Werte
-- (die Werte der Blätter des Expression-Baums)
values :: Expression -> [Int]
values (Value n)     = [n]
values (Apply _ x y) = values x ++ values y

-- um später alle möglichen Expressions aufzulisten
-- brauchen wir alle Teillisten der Verfügbaren
-- Zahlen (wobei hier die Anordnung bestehen bleiben soll)
subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = map (x:) (subs xs) ++ subs xs

-- um gleich perms zu implementieren können wir
-- pick benutzen: es liefert alle Möglichkeiten
-- ein Element aus einer Liste zu wählen (zusammen mit
-- den Rest)
-- Beispiel: pick [1,2,3] = [(1,[2,3]),(2,[1,3]),(3,[1,2])]
pick :: [a] -> [ (a,[a]) ]
pick [] = []
pick (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- pick xs ]

-- schließlich brauchen wir noch eine Funktion die uns
-- alle Permutationen (Umordnungen) einer Liste liefert
-- Tipp: Pick ist sicher nützlich ;)
perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = [ y:ys | (y,ys') <- pick xs, ys <- perms ys' ]

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
eval (Value n)      = [ n | n > 0 ]
eval (Apply op x y) = [ apply op a b | a <- eval x, b <- eval y, isValidOp op a b ]

-- subbags verbindet jetzt einfach perms und subs: damit bekommen
-- wir alle Permutationen aller Teillisten:
subbags :: [a] -> [[a]]
subbags xs = [ zs | ys <- subs xs, zs <- perms ys ]

-- diese Funktion liefert alle Möglichkeiten wie
-- eine Liste in zwei Teile geteilt werden kann
split :: [a] -> [ ([a],[a]) ]
split []     = [ ([],[]) ]
split (x:xs) = ([], x:xs) : [ (x:ls,rs) | (ls,rs) <- split xs ]

-- allerdings interessieren wir uns nur für die Aufteilungen
-- wo kein Teil leer ist
notEmptySplit :: [a] -> [ ([a],[a]) ]
notEmptySplit = filter notEmpty . split
  where notEmpty (xs,ys) = not (null xs || null ys)



-- um Schneller zu werden, erzeugen wir nicht nur die
-- Expressions sondern die Expressions + Ihren Wert
-- dabei filtern wir gleich diejenigen heraus, die
-- ungültig sind
type Result = (Expression, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [ (Value n, n) | n > 0 ]
results ns  = [ res | (ls,rs) <- notEmptySplit ns
                    , lx <- results ls
                    , ry <- results rs
                    , res <- combine lx ry ]
  where combine (l,x) (r,y) = [ (Apply op l r, apply op x y) | op <- ops, isValidOp op x y ]
        ops = [ Add, Sub, Mul, Div ]



-- außerdem schränken wir die gültigen Operationen noch weiter ein
-- um zu verhindern, dass wir 3+4 und 4+3 erzeugen, verlangen wird, dass
-- der erste Operand kleiner als der zweite sein soll, außerdem
-- machen Dinge wie *1 oder /1 keinen Unterschied
isValidOp :: (Ord a, Integral a) => Operand -> a -> a -> Bool
isValidOp Add x y = x <= y
isValidOp Sub x y = x > y
isValidOp Mul x y = x /= 1 && y /= 1 && x <= y
isValidOp Div x y = y /= 1 && x `mod` y == 0

solutions :: [Int] -> Int -> [Expression]
solutions ns n = [ e | ns' <- subbags ns, (e,m) <- results ns', m == n ]


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
