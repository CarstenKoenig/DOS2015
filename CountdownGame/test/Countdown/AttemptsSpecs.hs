{-# LANGUAGE OverloadedStrings #-}

module Countdown.AttemptsSpecs (main, spec) where

import Test.Hspec
import Countdown.Game (Player (..), Challange (..), Attempt(..), attemptFromFormula)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "wenn ein Spieler einen Vorschlag einreicht" $ do
    let player = Player "Testspieler" 1
    let challange = Challange 765 [1,3,7,10,25,25,50]
    context "und dabei eine valide Formel nur mit den gegebenen Zahlen benutzt" $ do
      let playerAttempt = "7*25 + 10*50"
          attmpt = attemptFromFormula challange player playerAttempt
      it "wird die Formel uebernommen" $ do
        formula attmpt `shouldBe` playerAttempt
      it "wird der Wert gesetzt" $ do
        value attmpt `shouldBe` Just 675
      it "wird die Differenz zum Zielwert gebildet" $ do
        difference attmpt `shouldBe` Just 90
      it "ist die Info 'OK'" $ do
        info attmpt `shouldBe` "OK"
    context "und die Formel einen Syntaxfehler enthaelt" $ do
      let playerAttempt = "7*25 +"
          attmpt = attemptFromFormula challange player playerAttempt
      it "wird die Formel uebernommen" $ do
        formula attmpt `shouldBe` playerAttempt
      it "wird der Wert nicht gesetzt" $ do
        value attmpt `shouldBe` Nothing
      it "wird die Differenz nicht gesetzt" $ do
        difference attmpt `shouldBe` Nothing
      it "ist die Info 'Syntaxfehler in Formel'" $ do
        info attmpt `shouldBe` "Syntaxfehler in Formel"
    context "und die Formel nicht den Regeln entspricht (Teilterm negativ)" $ do
      let playerAttempt = "7*(3-10)"
          attmpt = attemptFromFormula challange player playerAttempt
      it "wird die Formel uebernommen" $ do
        formula attmpt `shouldBe` playerAttempt
      it "wird der Wert nicht gesetzt" $ do
        value attmpt `shouldBe` Nothing
      it "wird die Differenz nicht gesetzt" $ do
        difference attmpt `shouldBe` Nothing
      it "ist die Info 'Formel enthaelt ungueltige Terme'" $ do
        info attmpt `shouldBe` "Formel enthaelt ungueltige Terme"
    context "die Formel nicht den Regeln entspricht (Teilen durch 0)" $ do
      let playerAttempt = "7/(25-25)"
          attmpt = attemptFromFormula challange player playerAttempt
      it "wird die Formel uebernommen" $ do
        formula attmpt `shouldBe` playerAttempt
      it "wird der Wert nicht gesetzt" $ do
        value attmpt `shouldBe` Nothing
      it "wird die Differenz nicht gesetzt" $ do
        difference attmpt `shouldBe` Nothing
      it "ist die Info 'Formel enthaelt ungueltige Terme'" $ do
        info attmpt `shouldBe` "Formel enthaelt ungueltige Terme"
    context "und die Formel nicht vorgegebene Zahlen enthaelt" $ do
      let playerAttempt = "7*5"
          attmpt = attemptFromFormula challange player playerAttempt
      it "wird die Formel uebernommen" $ do
        formula attmpt `shouldBe` playerAttempt
      it "wird der Wert nicht gesetzt" $ do
        value attmpt `shouldBe` Nothing
      it "wird die Differenz nicht gesetzt" $ do
        difference attmpt `shouldBe` Nothing
      it "ist die Info 'Formel darf nur die gegebenen Zahlen verwenden'" $ do
        info attmpt `shouldBe` "Formel darf nur die gegebenen Zahlen verwenden"
    context "und die Formel vorgegebene Zahlen zu oft enthaelt" $ do
      let playerAttempt = "25+25*25"
          attmpt = attemptFromFormula challange player playerAttempt
      it "wird die Formel uebernommen" $ do
        formula attmpt `shouldBe` playerAttempt
      it "wird der Wert nicht gesetzt" $ do
        value attmpt `shouldBe` Nothing
      it "wird die Differenz nicht gesetzt" $ do
        difference attmpt `shouldBe` Nothing
      it "ist die Info 'Formel darf nur die gegebenen Zahlen verwenden'" $ do
        info attmpt `shouldBe` "Formel darf nur die gegebenen Zahlen verwenden"
        
