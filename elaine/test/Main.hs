{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Text (pack)
import Elaine.AST
import Elaine.Eval (evalExpr, evalModule, newEnv)
import Elaine.Exec (exec)
import Elaine.Parse (ParseResult, parseExpr, parseProgram, prettyError)
import Test.Hspec
  ( Expectation,
    SpecWith,
    describe,
    hspec,
    it,
    shouldBe,
  )
import Text.RawString.QQ

tt :: Expr
tt = Val $ Bool True

ff :: Expr
ff = Val $ Bool False

iv :: Int -> Expr
iv = Val . Int

shouldBeP :: (Show a, Eq a) => ParseResult a -> a -> Expectation
result `shouldBeP` e = do
  case prettyError result of
    Left a -> putStr a
    Right a -> pure ()
  prettyError result `shouldBe` Right e

main :: IO ()
main = hspec $ do
  testParseExpr
  testParseProgram
  testEval
  testExec

testParseExpr :: SpecWith ()
testParseExpr = describe "parseExpr" $ do
  let f = parseExpr
  it "parses values" $ do
    f "false" `shouldBe` Right (Val $ Bool False)
    f "true" `shouldBe` Right (Val $ Bool True)
    f "10" `shouldBe` Right (Val $ Int 10)
    f "\"hello\"" `shouldBe` Right (Val $ String "hello")
    f "()" `shouldBe` Right (Val Unit)

  it "parses if expressions" $ do
    f "if true then false else true" `shouldBe` Right (If tt ff tt)
    f "if true then 5 else if false then 6 else 7" `shouldBe` Right (If tt (iv 5) (If ff (iv 6) (iv 7)))

  it "parses handle expressions" $ do
    f "handle h { if true then true else true }" `shouldBe` Right (Handle (Var "h") (If tt tt tt))

  it "parses let expressions" $ do
    f "let x = if true then true else true; x" `shouldBe` Right (Let "x" (If tt tt tt) (Var "x"))

  it "treats braces transparently" $ do
    f "{{{{ if {{ true }} then {{ true }} else {{ true }} }}}}" `shouldBe` Right (If tt tt tt)

testParseProgram :: SpecWith ()
testParseProgram = describe "parseProgram" $ do
  let f = parseProgram

  it "parses algebraic effects" $ do
    f
      [r|
      mod main {
        effect A {
          put(Int) ()
          get() Int
        }
      }
    |]
      `shouldBeP` [Mod "main" [Declaration Private $ DecEffect "A" [
        OperationSignature "put" [TypeName "Int"] UnitType,
        OperationSignature "get" [] (TypeName "Int")
      ]]]
  
  it "parses algebraic effects" $ do
    f
      [r|
      mod main {
        effect A! {
          put!(Int) ()
          get!() Int
        }
      }
    |]
      `shouldBeP` [Mod "main" [Declaration Private $ DecEffect "A!" [
        OperationSignature "put!" [TypeName "Int"] UnitType,
        OperationSignature "get!" [] (TypeName "Int")
      ]]]

testEval :: SpecWith ()
testEval = describe "eval0" $ do
  let f = evalExpr (newEnv "main")
  it "gives reduces if expressions" $ do
    f (If tt tt ff) `shouldBe` Bool True
    f (If ff tt ff) `shouldBe` Bool False
    f (If tt ff tt) `shouldBe` Bool False
    f (If ff ff tt) `shouldBe` Bool True

  it "binds simple lets" $ do
    f (Let "x" (iv 5) $ Var "x") `shouldBe` Int 5
    f (Let "x" (iv 5) $ Var "x") `shouldBe` Int 5

  it "keeps shadowing bindings intact" $ do
    f (Let "x" (iv 5) $ Let "x" (iv 6) (Var "x")) `shouldBe` Int 6

  it "applies function arguments" $ do
    f (App (Val $ Lam ["x"] $ Var "x") [iv 5]) `shouldBe` Int 5
    f (App (Val $ Lam ["y"] $ Var "y") [If tt (iv 5) (iv 6)]) `shouldBe` Int 5
    f (App (Val $ Lam ["x", "y"] (Var "y")) [iv 5, iv 10]) `shouldBe` Int 10

  it "applies multiple function arguments" $ do
    let g = Val $ Lam ["x", "y", "z"] (If (Var "x") (Var "y") (Var "z"))
    f (App g [tt, iv 100, iv 200]) `shouldBe` Int 100
    f (App g [ff, iv 100, iv 200]) `shouldBe` Int 200
    f (App g [App g [tt, tt, ff], iv (-10), iv 10]) `shouldBe` Int (-10)

  it "handles an abort-like effect" $ do
    -- Returns a boolean to signify whether it ran to completion or aborted
    let h = Val $ Hdl $ Handler (HandleReturn "x" tt) [OperationClause "abort" [] ff]
    f (Handle h (Let "x" (iv 5) (Var "x"))) `shouldBe` Bool True
    f (Handle h (Let "x" (App (Var "abort") []) (Var "x"))) `shouldBe` Bool False

testExec :: SpecWith ()
testExec = describe "exec" $ do
  let f x = fmap exec (parseProgram x)

  it "interprets a simple main module" $ do
    f "mod main { let main = 5 }" `shouldBe` Right (Int 5)
    f
      [r|
      mod main {
        let main = {
          let x = 5;
          let y = 6;
          if true then x else y
        }
      }
    |]
      `shouldBeP` Int 5

  it "elaborates higher order effects into values" $ do
    f
      [r|
      mod main {
        effect A! {
          a!() Int
        }

        elaboration A! -> <> {
          a!() {
            10
          }
        }

        let main = {
          elab { a!() }
        }
      }
    |]
      `shouldBeP` Int 10
  
  it "elaborates higher order effects into algebraic effects" $ do
    f [r|
      mod main {
        effect A {
          a() Int
        }

        effect B! {
          b!() Int
        }

        elaboration B! -> <A> {
          b!() {
            a()
          }
        }

        let main = {
          let h = handler {
            return(x) {
              x
            }
            a() {
              101
            }
          };
          handle h {
            elab {
              b!()
            }
          }
        }
      }
    |] `shouldBeP` Int 101

  it "can access global values" $ do
    f [r|
      mod main {
        let x = 5
        let y = 6
        let main = if true then x else y
      }
    |] `shouldBeP` Int 5 

  it "can use functions from the standard library" $ do
    f [r|
      mod main {
        import std
        let main = add(6, 5)
      }
    |] `shouldBeP` Int 11

    f [r|
      mod main {
        import std
        let main = sub(6, 5)
      }
    |] `shouldBeP` Int 1

    f [r|
      mod main {
        import std
        let main = mul(6, 5)
      }
    |] `shouldBeP` Int 30

    f [r|
      mod main {
        import std
        let main = concat(concat("hello", " "), "world")
      }
    |] `shouldBeP` String "hello world"

    f [r|
      mod main {
        import std
        let main = and(true, true)
      }
    |] `shouldBeP` Bool True

  it "a" $ do
    f [r|
      mod main {
        import std
        let main = {
          let x = add(5, 10);
          let y = add(x, 2);
          mul(2, y)
        }
      }
    |] `shouldBeP` Int 34

  it "handler the Out effect" $ do
    f [r|
      mod main {
        import std

        effect Out {
          write(String) ()
        }

        let h = handler {
          return(x) {
            ""
          }
          write(m) {
            let rest = resume(());
            concat(m, rest)
          }
        }

        let main = handle h {
          let x = write("hello");
          let x = write(" ");
          let x = write("world");
          ()
        }
      }
    |] `shouldBeP` String "hello world"

  it "can turn values into strings" $ do
    f [r|
      mod main {
        import std
        let main = show(5)
      }
    |] `shouldBeP` String "5"
