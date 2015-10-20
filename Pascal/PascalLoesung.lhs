Pascals Dreieck
===============

> module Pacal where
> import Data.List (unfoldr)

Ziel der Übung ist es das Pascalsche Dreieck:

```
    1
   1 1
  1 2 1
 1 3 3 1
1 4 6 4 1
```

in der Form

> erste5 = [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1]]

darzustellen.

Die Idee ist es, aus der vorhergehenden Zeile die nächst zu generieren:

> naechsteZeile :: [Int] -> [Int]

Wenn Du dir das Dreieck ansiehst, fällt Dir vielleicht auf, dass eine Zahl als Summe
aus den Zahlen die direkt oberhalb stehen gebildet werden können - nur die "Ränder" sind immer 1.

Es gibt verschiedene Arten wie ihr hier vorgehen könnt, ich empfehle aber folgendes zu versuchen:
Ausgehend von der vorhergehenden Zeile `ns=[n1,n2,n3,...nm]$, benutzt zwei *erweiterte* Versionen und
addiert diese mit `zipWith`:

```haskell
0  n1 n2 n3   ..  nm
n1 n2 n3 n4 .. nm 0
```

> naechsteZeile ns = zipWith (+) (0:ns) (ns++[0])

Das kannst Du jetzt direkt mit `unfoldr :: (b -> Maybe (a,b)) -> b -> [a]` verwenden um das Dreieck zu erzeugen:

> pascals :: [[Int]]
> pascals = unfoldr (\ ns -> Just (ns, naechsteZeile ns)) [1]

Wenn Du alles richtig gemacht hast, sollte ein aufruf von `:main` in GHCi `OK` ausgeben

> main :: IO ()
> main = do
>   if take 5 pascals == erste5
>     then print "OK"
>     else print "leider falsch" 
