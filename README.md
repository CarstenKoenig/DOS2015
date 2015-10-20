# DOS2015
Haskell Workshop Übungsmaterial für den DOS2015

### Update
Ich habe die entsprechenden Lösungen hinzugefügt.

Außerdem findet ihr hier eine C# Version des Countdown-Problems

**Achtung:**
diese benutzt exzessiv `IEnumerable` ist also nicht unbedingt sehr perfomant - dafür ist sie denke ich sehr nahe am Haskell-Code).

---

Außerdem findet habe ich noch die [Web-Spiel-Version](./CountdownGame) mit in das Repository gepack.

Um das zu Kompilieren/Starten solltet ihr so vorgehen:

- Sorgt dafür, dass ihr SQLite auf euerem System habt (in der Regel reicht die entsprechende .DLL
- in einem Terminal/auf einer Konsole
- navigiert in das Verzeichnis `cd CountdownGame`
- initialisiert am besten eine Sandbox: `cabal sandbox init`
- führt ein Update der cabal Datenbank durch `cabal update`
- installiert die Dependencies (inkl. Testsupport): `cabal install --dependencies-only --enable-tests` (das wird eine Weile dauern...)
- führt die Tests durch, wenn ihr wollt: `cabal test`
- Ansonsten startet das *Spiel* mt `cabal run`
- Wenn alles gut ist solltet ihr dann im Browser [unter localhost:8080/play](http://localhost:8080/play) loslegen können. (Etwas Gedult eine Runde startet nach 30sek. und geht 60sek.!)
