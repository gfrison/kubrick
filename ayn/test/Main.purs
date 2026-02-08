module Test.Ayn.Main where

import Prelude

import Ayn (parse)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Effect (Effect)
import Kubrick.Lem (Lem(..))
import Kubrick.Types (Raw(..))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
  describe "Ayn" do
    describe "parse" do
      describe "basic L1" do
        it "parses single word" do
          parse "a" `shouldEqual` Right { facts: (L1 (Rs "a") : Nil) }
        
        it "parses quoted string" do
          parse "\"a b c\"" `shouldEqual` Right { facts: (L1 (Rs "a b c") : Nil) }
        
        it "parses integer" do
          parse "42" `shouldEqual` Right { facts: (L1 (Ri 42) : Nil) }
        
        it "parses float" do
          parse "3.14" `shouldEqual` Right { facts: (L1 (Rf 3.14) : Nil) }
        
        it "parses true" do
          parse "true" `shouldEqual` Right { facts: (L1 (Rb true) : Nil) }
        
        it "parses false" do
          parse "false" `shouldEqual` Right { facts: (L1 (Rb false) : Nil) }
      
      describe "Sek" do
        it "parses two elements" do
          parse "a b" `shouldEqual` 
            Right { facts: (Sek (L1 (Rs "a")) (L1 (Rs "b")) Nil : Nil) }
        
        it "parses three elements" do
          parse "a b c" `shouldEqual` 
            Right { facts: (Sek (L1 (Rs "a")) (L1 (Rs "b")) ((L1 (Rs "c")) : Nil) : Nil) }
        
        it "parses with square brackets" do
          parse "a [b c] d e" `shouldEqual` 
            Right { facts: (
              Sek (L1 (Rs "a")) 
                  (Sek (L1 (Rs "b")) (L1 (Rs "c")) Nil) 
                  ((L1 (Rs "d")) : (L1 (Rs "e")) : Nil) : Nil) }
        
        it "parses with round brackets" do
          parse "a (b c) d e" `shouldEqual` 
            Right { facts: (
              Sek (L1 (Rs "a")) 
                  (Sek (L1 (Rs "b")) (L1 (Rs "c")) Nil) 
                  ((L1 (Rs "d")) : (L1 (Rs "e")) : Nil) : Nil) }
      
      describe "Pair" do
        it "parses simple pair" do
          parse "a -> b" `shouldEqual` 
            Right { facts: (Pair (L1 (Rs "a")) (L1 (Rs "b")) : Nil) }
      
      describe "Bag" do
        it "parses bag with two elements" do
          parse "{a b}" `shouldEqual` 
            Right { facts: (Bag (L1 (Rs "a")) (L1 (Rs "b")) Nil : Nil) }
        
        it "parses bag with nested sek" do
          parse "{a b [c d]}" `shouldEqual` 
            Right { facts: (
              Bag (L1 (Rs "a")) 
                  (L1 (Rs "b")) 
                  ((Sek (L1 (Rs "c")) (L1 (Rs "d")) Nil) : Nil) : Nil) }
      
      describe "Choice" do
        it "parses choice with semicolons" do
          parse "a b;c;d e" `shouldEqual` 
            Right { facts: (
              Sek (L1 (Rs "a")) 
                  (Choice (L1 (Rs "b")) (L1 (Rs "c")) ((L1 (Rs "d")) : Nil)) 
                  ((L1 (Rs "e")) : Nil) : Nil) }
        
        it "parses choice with parentheses" do
          parse "a (b;c;d) e" `shouldEqual` 
            Right { facts: (
              Sek (L1 (Rs "a")) 
                  (Choice (L1 (Rs "b")) (L1 (Rs "c")) ((L1 (Rs "d")) : Nil)) 
                  ((L1 (Rs "e")) : Nil) : Nil) }
      
      describe "multiline" do
        it "parses multiline with indentation" do
          parse "a b c\n  d e f" `shouldEqual` 
            Right { facts: (
              Sek (L1 (Rs "a")) 
                  (L1 (Rs "b")) 
                  ((L1 (Rs "c")) : (L1 (Rs "d")) : (L1 (Rs "e")) : (L1 (Rs "f")) : Nil) : Nil) }
        
        it "parses multiline without indentation as separate facts" do
          parse "a b c\nd e f" `shouldEqual` 
            Right { facts: (
              (Sek (L1 (Rs "a")) (L1 (Rs "b")) ((L1 (Rs "c")) : Nil)) : 
              (Sek (L1 (Rs "d")) (L1 (Rs "e")) ((L1 (Rs "f")) : Nil)) : 
              Nil) }

