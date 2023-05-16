{-# LANGUAGE QuasiQuotes #-}

module TypeCheck (testTypeCheck) where

import Data.Either (isLeft)
import Data.Text (pack)
import Elaine.AST ( ValueType(TypeHandler, TypeInt, TypeBool, TypeString), ComputationType(ComputationType), EffectRow (Empty) )
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

check :: String -> Result ComputationType
check = execCheck . pack' . (,) "test"

testTypeCheck :: SpecWith ()
testTypeCheck = describe "typeCheck" $ do
  it "checks an if" $ do
    check [r|
      let main = if true { 5 } else { 10 };
    |]
      `shouldBe` Right (ComputationType Empty TypeInt)

  it "checks bindings" $ do
    check [r|
      let x = "Hello";
      let main = x;
    |]
      `shouldBe` Right (ComputationType Empty TypeString)

  it "checks function applications" $ do
    check [r|
      let f = fn(c, x, y) {
        if c { x } else { y }
      };
      let main = f(true, 5, 10);
    |]
      `shouldBe` Right (ComputationType Empty TypeInt)

  it "can type check used items" $ do
    check [r|
      mod A {
        pub let x = 5;
      }
      use A;
      let main = x;
    |]
      `shouldBe` Right (ComputationType Empty TypeInt)

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
    |] `shouldBe` Right (ComputationType Empty TypeInt)
    
    check [r|
      use std;
      let main = concat("hello", concat(" ", " world"));
    |] `shouldBe` Right (ComputationType Empty TypeString)
  
  it "checks input for built-ins" $ do
    check [r|
      use std;
      let main = add("hello", 4);
    |] `shouldSatisfy` isTypeError
  
  it "respects let type annotations" $ do
    check [r|
      let main: Int = "hello";
    |] `shouldSatisfy` isTypeError
    
    check [r|
      let main: String = "hello";
    |] `shouldBe` Right (ComputationType Empty TypeString)
  
  it "cannot assign mono to poly" $ do
    check [r|
      let main: a = "hello";
    |] `shouldSatisfy` isTypeError
  
  it "can assign to bound poly" $ do
    check [r|
      let f = fn(x: a) {
        let y = x;
        y
      };
      let main = f(5);
    |] `shouldBe` Right (ComputationType Empty TypeInt)
  
  it "can assign to explicit bound poly" $ do
    check [r|
      let f = fn(x: a) {
        let y: a = x;
        y
      };
      let main = f(5);
    |] `shouldBe` Right (ComputationType Empty TypeInt)
  
  it "cannot use poly as specific type" $ do
    check [r|
      let f = fn(x: a) {
        add(x, x)
      };
      let main = f(2);
    |] `shouldSatisfy` isTypeError
  
  it "uses the function body to infer types" $ do
    check [r|
      let f = fn(x) {
        add(x, x)
      };
      let main = f("hello");
    |] `shouldSatisfy` isTypeError

  it "respects function type annotations" $ do
    check [r|
      let f = fn(x: Int) Int {
        x
      };

      let main = f(5);
    |] `shouldBe` Right (ComputationType Empty TypeInt)
    
    check [r|
      let f = fn(x: Int) Int {
        x
      };

      let main = f("hello");
    |] `shouldSatisfy` isTypeError

    check [r|
      let f = fn(x) Int {
        x
      };

      let main = f("hello");
    |] `shouldSatisfy` isTypeError
  
  it "checks function types" $ do
    check [r|
      let f: fn(Int) Int = fn(x) { x };
      let main = f(5);
    |] `shouldBe` Right (ComputationType Empty TypeInt)
    
    check [r|
      let f: fn(Int) Int = fn(x) { x };
      let main = f("hello");
    |] `shouldSatisfy` isTypeError 

  it "checks first class functions" $ do
    check [r|
      use std;
      let f = fn(x) fn(Int) Int {
        fn(y) {
          add(x, y)
        }
      };
      let main = f(1)(2);
    |] `shouldBe` Right (ComputationType Empty TypeInt)
    
    check [r|
      use std;
      let f = fn(x) fn(Int) Int {
        fn(y) {
          add(x, y)
        }
      };
      let main = f("hello")(2);
    |] `shouldSatisfy` isTypeError
    
    check [r|
      use std;
      let f = fn(x) fn(Int) Int {
        fn(y) {
          add(x, y)
        }
      };
      let main = f(2)("hello");
    |] `shouldSatisfy` isTypeError
  
  it "can do polymorphic functions" $ do
    check [r|
      let f = fn(x) {
        x
      };
      let main = {
        let a = f(2);
        f("hello")
      };
    |] `shouldBe` Right (ComputationType Empty TypeString)

  it "can call effectless functions in main" $ do
    check [r|
      let f = fn(x: a) <> a {
        x
      };
      let main = f(5);
    |] `shouldBe` Right (ComputationType Empty TypeInt)

  it "cannot call effectful functions in main" $ do
    check [r|
      let f = fn(x: a) <A> a {
        x
      };
      let main = f(5);
    |] `shouldSatisfy` isTypeError
  
  it "can infer handler types" $ do
    check [r|
      effect Foo {
        bar(a) a
      }

      let main = handler {
        return(x) { x }
        bar(x) { resume(x) } 
      };
    |] `shouldSatisfy` \case
        Right (ComputationType Empty (TypeHandler "Foo" _ _)) -> True
        _ -> False
  
  it "can infer handler types with specific types in the operations" $ do
    check [r|
      use std;

      effect Foo {
        bar(Int) Int
      }

      let main = handler {
        return(x) { x }
        bar(x) { resume(add(x, x)) } 
      };
    |] `shouldSatisfy` \case
        Right (ComputationType Empty (TypeHandler "Foo" _ _)) -> True
        _ -> False

  it "can cannot be more specific about types in handlers" $ do
    check [r|
      use std;

      effect Foo {
        bar(a) a
      }

      let main = handler {
        return(x) { x }
        bar(x) { add(x, x) } 
      };
    |] `shouldSatisfy` isTypeError
  
  it "handler return types must match" $ do
    check [r|
      effect Foo {
        bar(a) a
      }

      let main = handler {
        return(x) { "hello" }
        bar(x) { 5 }
      };
    |] `shouldSatisfy` isTypeError
    
    check [r|
      effect Foo {
        bar(a) a
      }

      let main = handler {
        return(x) { "hello" }
        bar(x) { "world" }
      };
    |] `shouldSatisfy` \case
        Right (ComputationType Empty (TypeHandler "Foo" _ TypeString)) -> True
        _ -> False
  
  it "can apply a handler" $ do
    check [r|
      effect Foo {
        bar(a) a
      }

      let h = handler {
        return(x) { "hello" }
        bar(x) { "world" }
      };

      let main = handle[h] {
        5
      };
    |] `shouldBe` Right (ComputationType Empty TypeString)
  
  it "cannot use operation outside of handle" $ do
    check [r|
      effect Foo {
        bar(a) a
      }

      let main = bar(5);
    |] `shouldSatisfy` isTypeError
  
  it "cannot use operation in effectless function" $ do
    check [r|
      effect Foo {
        bar(a) a
      }
      
      let f = fn() <|e> Int {
        bar(5)
      };

      let h = handler {
        return(x) { "hello" }
        bar(x) { "world" }
      };

      let main = handle[h] { f() };
    |] `shouldSatisfy` isTypeError
    
    check [r|
      effect Foo {
        bar(a) a
      }
      
      let f = fn() <Foo|e> Int {
        bar(5)
      };

      let h = handler {
        return(x) { "hello" }
        bar(x) { "world" }
      };

      let main = handle[h] { f() };
    |] `shouldBe` Right (ComputationType Empty TypeString)
  
  it "type for resume must match return type of operation" $ do
    check [r|
      effect Foo {
        bar() Int
      }

      let h = handler {
        return(x) { x }
        bar() {
          resume(5)
        }
      };

      let main = handle[h] bar();
    |] `shouldBe` Right (ComputationType Empty TypeInt)
    
    check [r|
      effect Foo {
        bar() Int
      }

      let h = handler {
        return(x) { x }
        bar() {
          resume("hello")
        }
      };

      let main = handle[h] bar();
    |] `shouldSatisfy` isTypeError
  
  it "can use effects in function application" $ do
    check [r|
      use std;
      effect Foo {
        foo() Int
      }

      let hf = handler {
        return(x) { x }
        foo() { resume(2) }
      };

      let main = handle[hf] add(foo(), foo());
    |] `shouldBe` Right (ComputationType Empty TypeInt)