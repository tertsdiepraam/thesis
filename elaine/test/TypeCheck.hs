{-# LANGUAGE QuasiQuotes #-}

module TypeCheck (testTypeCheck) where

import Data.Either (isLeft)
import Data.Text (pack)
import Elaine.AST
import Elaine.ElabTransform (elabTrans)
import Elaine.Exec (exec, Result, pack', execCheck, isTypeError)
import Elaine.Parse (ParseResult, parseExpr, parseProgram)
import Elaine.Pretty (pretty)
import Elaine.TypeCheck (TypeEnv (TypeEnv), getMain, getVar, typeCheck)
import Test.Hspec
  ( Expectation,
    SpecWith,
    describe,
    expectationFailure,
    hspec,
    it,
    shouldBe,
    shouldSatisfy,
  )
import Test.Hspec.Runner (SpecResult (specResultSuccess))
import Text.RawString.QQ (r)

check :: String -> Result ValueType
check = execCheck . pack' . (,) "test"

testTypeCheck :: SpecWith ()
testTypeCheck = describe "typeCheck" $ do
  it "checks an if" $ do
    check [r|
      let main = if true { 5 } else { 10 };
    |]
      `shouldBe` Right TypeInt

  it "checks bindings" $ do
    check [r|
      let x = "Hello";
      let main = x;
    |]
      `shouldBe` Right TypeString

    check [r|
      let main = {
        let x = true;
        x
      };
    |]
      `shouldBe` Right TypeBool

  it "checks function applications" $ do
    check [r|
      let f = fn(c, x, y) {
        if c { x } else { y }
      };
      let main = f(true, 5, 10);
    |]
      `shouldBe` Right TypeInt

  it "can type check used items" $ do
    check [r|
      mod A {
        pub let x = 5;
      }
      use A;
      let main = x;
    |]
      `shouldBe` Right TypeInt

  it "errors on undefined variable" $ do
    check
      [r|
      let main = x;
    |] `shouldSatisfy` isTypeError 

  it "errors on if with branches of different types" $ do
    check [r|
      let main = if true { 5 } else { "hello" };
    |] `shouldSatisfy` isTypeError

  it "errors on if with non-bool condition" $ do
    check
      [r|
      let main = if 1 { 5 } else { 10 };
    |] `shouldSatisfy` isTypeError
  
  it "gets types from std" $ do
    check [r|
      use std;
      let main = add(1, 2);
    |] `shouldBe` Right TypeInt
    
    check [r|
      use std;
      let main = concat("hello", concat(" ", " world"));
    |] `shouldBe` Right TypeString
  
  it "checks input for built-ins" $ do
    check [r|
      use std;
      let main = add("hello", 4);
    |] `shouldSatisfy` isTypeError