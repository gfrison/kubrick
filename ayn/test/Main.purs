module Test.FactParser.Main where

import Prelude

import Bundler (bundle)
import Data.Either (Either(..), isLeft)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import FactParser (parse)
import Kubrick.Kube (get)
import Kubrick.Kube.Types (Kid(..))
import Kubrick.Lem (Lem(..), (<+>), (+:),(:::), (:+), (\/))
import Kubrick.Types (Raw(..), Atom(..), Var(..))
import Parser (parseMethod, parseProgram, parseAtom)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, fail)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Types (Bundles(..), Method(..), Program(..))

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
  describe "FactParser" do
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
            Right { facts: ((Rs "a" +: L1 (Rs "b")) : Nil) }
        
        it "parses three elements" do
          parse "a b c" `shouldEqual` 
            Right { facts: ((Rs "a" +: Rs "b" +: L1 (Rs "c")) : Nil) }
        
        it "parses with square brackets" do
          parse "a [b c] d e" `shouldEqual` 
            Right { facts: (
              Sek (L1 (Rs "a")) 
                  (Rs "b" +: L1 (Rs "c")) 
                  ((L1 (Rs "d")) : (L1 (Rs "e")) : Nil) : Nil) }
        
        it "parses with round brackets" do
          parse "a (b c) d e" `shouldEqual` 
            Right { facts: (((L1 (Rs "a")) ::: (Rs "b" +: L1 (Rs "c"))) :+ Rs "d" :+ Rs "e") : Nil}
      
      describe "Pair" do
        it "parses simple pair" do
          parse "a -> b" `shouldEqual` 
            Right { facts: (Pair (L1 (Rs "a")) (L1 (Rs "b")) : Nil) }
      
      describe "Bag" do
        it "parses bag with two elements" do
          parse "{a b}" `shouldEqual` 
            Right { facts: ((L1 (Rs "a") <+> L1 (Rs "b")) : Nil) }
        
        it "parses bag with nested sek" do
          parse "{a b [c d]}" `shouldEqual` 
            Right { facts: (
              Bag (L1 (Rs "a")) 
                  (L1 (Rs "b")) 
                  ((Rs "c" +: L1 (Rs "d")) : Nil) : Nil) }
      
      describe "Choice" do
        it "parses choice with semicolons" do
          parse "a b;c;d e" `shouldEqual` 
            Right { facts: (
              (((L1 (Rs "a")) ::: ((Rs "b") \/ (Rs "c") \/ Rs "d")) :+ (Rs "e")) : Nil) }
        
        it "parses choice with parentheses" do
          parse "a (b;c;d) e" `shouldEqual` 
            Right { facts: (
                (((L1 (Rs "a")) ::: ((Rs "b") \/ (Rs "c") \/ Rs "d")) :+ (Rs "e")) : Nil) }
      
      describe "multiline" do
        it "parses multiline with indentation" do
          parse "a b c\n  d e f" `shouldEqual` 
            Right { facts: ((
              Rs "a" +: 
                  Rs "b" +: 
                  Rs "c" +: Rs "d" +: Rs "e" +: L1 (Rs "f")) : Nil) }
        
        it "parses multiline without indentation as separate facts" do
          parse "a b c\nd e f" `shouldEqual` 
            Right { facts: (
              (Rs "a" +: Rs "b" +: L1 (Rs "c")) : 
              (Rs "d" +: Rs "e" +: L1 (Rs "f")) : 
              Nil) }

  describe "parseAtom" do
    it "parses lowercase word as Ar Raw" do
      parseAtom "hello" `shouldEqual` Right (L1 (Ar (Rs "hello")))

    it "parses integer as Ar Raw" do
      parseAtom "42" `shouldEqual` Right (L1 (Ar (Ri 42)))

    it "parses float as Ar Raw" do
      parseAtom "3.14" `shouldEqual` Right (L1 (Ar (Rf 3.14)))

    it "parses boolean as Ar Raw" do
      parseAtom "true" `shouldEqual` Right (L1 (Ar (Rb true)))

    it "parses capitalized word as Av Var" do
      parseAtom "X" `shouldEqual` Right (L1 (Av (Sar "X")))

    it "parses multi-char capitalized word as Av Var" do
      parseAtom "Var1" `shouldEqual` Right (L1 (Av (Sar "Var1")))

    it "parses underscore as Gap" do
      parseAtom "_" `shouldEqual` Right Gap

    it "parses quoted string as Ar Raw" do
      parseAtom "\"hello world\"" `shouldEqual` Right (L1 (Ar (Rs "hello world")))

  describe "parseMethod" do
    it "parses Fun method with =|" do
      parseMethod "a X =| b X" `shouldEqual`
        Right (Fun { head: Ar (Rs "a") +: L1 (Av (Sar "X"))
                   , body: Ar (Rs "b") +: L1 (Av (Sar "X")) })

    it "parses Impl method with -|" do
      parseMethod "a X -| b X" `shouldEqual`
        Right (Impl { head: Ar (Rs "a") +: L1 (Av (Sar "X"))
                    , body: Ar (Rs "b") +: L1 (Av (Sar "X")) })

    it "parses method with Gap (_)" do
      parseMethod "a _ =| b" `shouldEqual`
        Right (Fun { head: Sek (L1 (Ar (Rs "a"))) Gap Nil
                   , body: L1 (Ar (Rs "b")) })

    it "parses method with complex Lem" do
      parseMethod "f X Y =| g X [h Y]" `shouldEqual`
        Right (Fun { head: Ar (Rs "f") +: Av (Sar "X") +: L1 (Av (Sar "Y"))
                   , body: Sek (L1 (Ar (Rs "g"))) 
                               (L1 (Av (Sar "X"))) 
                               ((Ar (Rs "h") +: L1 (Av (Sar "Y"))) : Nil) })

    it "fails on missing separator" do
      parseMethod "a b c" `shouldSatisfy` isLeft

    it "parses method with raw values in body" do
      parseMethod "add X Y =| result 42" `shouldEqual`
        Right (Fun { head: Ar (Rs "add") +: Av (Sar "X") +: L1 (Av (Sar "Y"))
                   , body: Ar (Rs "result") +: L1 (Ar (Ri 42)) })

  describe "parseProgram" do
    it "parses facts only" do
      parseProgram "a b\nc d" `shouldEqual`
        Right (Program { facts: (Rs "a" +: L1 (Rs "b")) : (Rs "c" +: L1 (Rs "d")) : Nil
              , queries: Nil
              , methods: Nil })

    it "parses queries with ? prefix" do
      parseProgram "?a X" `shouldEqual`
        Right (Program { facts: Nil
              , queries: (Ar (Rs "a") +: L1 (Av (Sar "X"))) : Nil
              , methods: Nil })

    it "parses methods" do
      parseProgram "f X =| g X" `shouldEqual`
        Right (Program { facts: Nil
              , queries: Nil
              , methods: (Fun { head: Ar (Rs "f") +: L1 (Av (Sar "X"))
                              , body: Ar (Rs "g") +: L1 (Av (Sar "X")) }) : Nil })

    it "parses mixed program" do
      parseProgram "a b\nf X =| g X\n?h Y" `shouldEqual`
        Right (Program { facts: (Rs "a" +: L1 (Rs "b")) : Nil
              , queries: (Ar (Rs "h") +: L1 (Av (Sar "Y"))) : Nil
              , methods: (Fun { head: Ar (Rs "f") +: L1 (Av (Sar "X"))
                              , body: Ar (Rs "g") +: L1 (Av (Sar "X")) }) : Nil })

    it "rejects facts containing variables" do
      parseProgram "a X" `shouldSatisfy` isLeft

    it "parses empty program" do
      parseProgram "" `shouldEqual`
        Right (Program { facts: Nil, queries: Nil, methods: Nil })

    it "parses program with Impl method" do
      parseProgram "a X -| b X" `shouldEqual`
        Right (Program { facts: Nil
              , queries: Nil
              , methods: (Impl { head: Ar (Rs "a") +: L1 (Av (Sar "X"))
                               , body: Ar (Rs "b") +: L1 (Av (Sar "X")) }) : Nil })

    it "parses multiline method with indentation" do
      parseProgram "f X =| g X\n  h Y" `shouldEqual`
        Right (Program { facts: Nil
              , queries: Nil
              , methods: (Fun { head: Ar (Rs "f") +: L1 (Av (Sar "X"))
                              , body: Ar (Rs "g") +: Av (Sar "X") +: Ar (Rs "h") +: L1 (Av (Sar "Y")) }) : Nil })

  describe "Bundler" do
    it "bundles empty program" do
      let result = bundle (Program { facts: Nil, queries: Nil, methods: Nil })
      case result of
        Right (Bundles b) -> do
          b.queries `shouldEqual` Nil
          Set.size b.facts.roots `shouldEqual` 0
          Set.size b.methods.roots `shouldEqual` 0
        Left err -> fail err

    it "bundles facts into Kube" do
      let fact1 = Rs "a" +: L1 (Rs "b")
          fact2 = Rs "c" +: Rs "d" +: L1 (Rs "e")
          prog = Program { facts: fact1 : fact2 : Nil, queries: Nil, methods: Nil }
      case bundle prog of
        Right (Bundles b) -> do
          Set.size b.facts.roots `shouldEqual` 2
          get b.facts (Kid 0) `shouldEqual` Just fact1
          get b.facts (Kid 1) `shouldEqual` Just fact2
        Left err -> fail err

    it "bundles queries unchanged" do
      let query = Ar (Rs "a") +: L1 (Av (Sar "X"))
          prog = Program { facts: Nil, queries: query : Nil, methods: Nil }
      case bundle prog of
        Right (Bundles b) -> b.queries `shouldEqual` (query : Nil)
        Left err -> fail err

    it "bundles Fun method as Bag" do
      let method = Fun { head: Ar (Rs "f") +: L1 (Av (Sar "X"))
                       , body: Ar (Rs "g") +: L1 (Av (Sar "X")) }
          prog = Program { facts: Nil, queries: Nil, methods: method : Nil }
      case bundle prog of
        Right (Bundles b) -> do
          Set.size b.methods.roots `shouldEqual` 1
          let expected = Dict
                (Tuple (L1 (Rs "type")) (L1 (Rs "fun")))
                (Tuple (L1 (Rs "head")) (Rs "f" +: L1 (Rs "X")))
                (Tuple (L1 (Rs "body")) (Rs "g" +: L1 (Rs "X")) : Nil)
          -- Find the root Kid and verify
          case Set.findMin b.methods.roots of
            Just rootKid -> get b.methods rootKid `shouldEqual` Just expected
            Nothing -> fail "No root Kid found"
        Left err -> fail err

    it "bundles Impl method as Bag" do
      let method = Impl { head: Ar (Rs "a") +: L1 (Av (Sar "Y"))
                        , body: Ar (Rs "b") +: L1 (Av (Sar "Y")) }
          prog = Program { facts: Nil, queries: Nil, methods: method : Nil }
      case bundle prog of
        Right (Bundles b) -> do
          Set.size b.methods.roots `shouldEqual` 1
          let expected = Dict
                (Tuple (L1 (Rs "type")) (L1 (Rs "impl")))
                (Tuple (L1 (Rs "head")) (Rs "a" +: L1 (Rs "Y")))
                (Tuple (L1 (Rs "body")) (Rs "b" +: L1 (Rs "Y")) : Nil)
          case Set.findMin b.methods.roots of
            Just rootKid -> get b.methods rootKid `shouldEqual` Just expected
            Nothing -> fail "No root Kid found"
        Left err -> fail err

    it "bundles mixed program" do
      let fact = Rs "x" +: L1 (Rs "y")
          query = Ar (Rs "q") +: L1 (Av (Sar "Z"))
          method = Fun { head: L1 (Ar (Rs "m")), body: L1 (Ar (Rs "n")) }
          prog = Program { facts: fact : Nil, queries: query : Nil, methods: method : Nil }
      case bundle prog of
        Right (Bundles b) -> do
          Set.size b.facts.roots `shouldEqual` 1
          Set.size b.methods.roots `shouldEqual` 1
          b.queries `shouldEqual` (query : Nil)
          get b.facts (Kid 0) `shouldEqual` Just fact
        Left err -> fail err

    it "allows duplicate facts" do
      let fact = L1 (Rs "dup")
          prog = Program { facts: fact : fact : Nil, queries: Nil, methods: Nil }
      case bundle prog of
        Right (Bundles b) -> do
          Set.size b.facts.roots `shouldEqual` 2
        Left err -> fail err
