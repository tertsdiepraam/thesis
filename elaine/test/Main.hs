{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Text (pack)
import Elaine.AST
import Elaine.Eval (evalExpr, evalModule, newEnv)
import Elaine.Exec (exec)
import Elaine.Parse (ParseResult, parseExpr, parseProgram, prettyError)
import Elaine.Pretty (pretty)
import Test.Hspec
  ( Expectation,
    SpecWith,
    describe,
    hspec,
    it,
    shouldBe,
  )
import Text.RawString.QQ
import Elaine.ElabTransform (elabTrans)

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

-- Check transformed
shouldEvalTo :: String -> Value -> Expectation
x `shouldEvalTo` e = case prettyError (parseProgram x) of
    Left a -> putStr a
    Right parsed -> do
      putStrLn "======= Original ======="
      putStrLn (pretty parsed)
      exec parsed `shouldBe` e
      let transformed = elabTrans parsed
      putStrLn "====== Transformed ======"
      putStrLn (pretty transformed)
      exec transformed `shouldBe` e

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
    f "let x = if true then true else true\nx" `shouldBe` Right (Let "x" (If tt tt tt) (Var "x"))

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
      `shouldBeP` [ Mod
                      "main"
                      [ Declaration Private $
                          DecEffect
                            "A"
                            [ OperationSignature "put" [TypeName "Int"] UnitType,
                              OperationSignature "get" [] (TypeName "Int")
                            ]
                      ]
                  ]

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
      `shouldBeP` [ Mod
                      "main"
                      [ Declaration Private $
                          DecEffect
                            "A!"
                            [ OperationSignature "put!" [TypeName "Int"] UnitType,
                              OperationSignature "get!" [] (TypeName "Int")
                            ]
                      ]
                  ]

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
  it "interprets a simple main module" $ do
    "mod main { let main = 5 }"
      `shouldEvalTo` Int
        5
    
    [r|
      mod main {
        let main = {
          let x = 5
          let y = 6
          if true then x else y
        }
      }
    |]
      `shouldEvalTo` Int 5

  it "elaborates higher order effects into values" $ do
    [r|
      mod main {
        effect A! {
          a!() Int
        }

        let elabA = elaboration A! -> <> {
          a!() {
            10
          }
        }

        let main = {
          elab[elabA] { a!() }
        }
      }
    |]
      `shouldEvalTo` Int 10

  it "elaborates higher order effects into algebraic effects" $ do
    [r|
      mod main {
        effect A {
          a() Int
        }

        effect B! {
          b!() Int
        }

        let eB = elaboration B! -> <A> {
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
          }
          handle h {
            elab[eB] {
              b!()
            }
          }
        }
      }
    |]
      `shouldEvalTo` Int 101

  it "can access global values" $ do
    [r|
      mod main {
        let x = 5
        let y = 6
        let main = if true then x else y
      }
    |]
      `shouldEvalTo` Int 5

  it "can use functions from the standard library" $ do
    [r|
      mod main {
        import std
        let main = add(6, 5)
      }
    |]
      `shouldEvalTo` Int 11

    [r|
      mod main {
        import std
        let main = sub(6, 5)
      }
    |]
      `shouldEvalTo` Int 1

    [r|
      mod main {
        import std
        let main = mul(6, 5)
      }
    |]
      `shouldEvalTo` Int 30
    
    [r|
      mod main {
        import std
        let main = concat(concat("hello", " "), "world")
      }
    |]
      `shouldEvalTo` String "hello world"

    [r|
      mod main {
        import std
        let main = and(true, true)
      }
    |]
      `shouldEvalTo` Bool True

  it "a" $ do
    [r|
      mod main {
        import std
        let main = {
          let x = add(5, 10)
          let y = add(x, 2)
          mul(2, y)
        }
      }
    |]
      `shouldEvalTo` Int 34

  it "handler the Out effect" $ do
    [r|
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
            let rest = resume(())
          }
        }

        let main = handle h {
          let x = write("hello")
          let x = write(" ")
          let x = write("world")
          ()
        }
      }
    |]
      `shouldEvalTo` String "hello world"

  it "can turn values into strings" $ do
    [r|
      mod main {
        import std
        let main = show(5)
      }
    |]
      `shouldEvalTo` String "5"

  it "can construct data types" $ do
    [r|
      mod main {
        type Either {
          Left(<> a)
          Right(<> b)
        }

        let main = Left(5)
      }
    |]
      `shouldEvalTo` Data "Either" "Left" [Val $ Int 5]

  it "can represent a list" $ do
    [r|
      mod main {
        type List {
          Nil()
          Cons(<> a, <> List)
        }

        let main = Cons(1, Cons(2, Nil()))
      }
    |]
      `shouldEvalTo` Data "List" "Cons" [Val $ Int 1, Val $ Data "List" "Cons" [Val $ Int 2, Val $ Data "List" "Nil" []]]

  it "can match on custom data types" $ do
    [r|
      mod main {
        type Bool {
          False()
          True()
        }

        let main = match True() {
          True() => 5
          False() => 10
        }
      }
    |]
      `shouldEvalTo` Int 5

  it "can call a defined function" $ do
    [r|
      mod main {
        import std
        let add3 = fn(a: <>Int, b: <>Int, c: <>Int) <> Int {
          add(a, add(b, c))
        }

        let main = add3(1, 2, 3)
      }
    |]
      `shouldEvalTo` Int 6

  it "can call a function returning a custom datatype" $ do
    [r|
      mod main {
        import std

        type Maybe {
          Just(<> a)
          Nothing()
        }

        let safediv = fn(x: <>Int, y: <>Int) <> Maybe {
          if eq(y, 0) then
            Nothing()
          else
            Just(div(x,y))
        }

        let main = safediv(6, 0)
      }
    |]
      `shouldEvalTo` Data "Maybe" "Nothing" []

  it "can handle the abort effect" $ do
    [r|
      mod main {
        import std
        
        type Maybe {
          Just(<> a)
          Nothing()
        }

        effect Abort {
          abort() ()
        }

        let hAbort = handler {
          return(x) { Just(x) }
          abort() { Nothing() }
        }

        let safediv = fn(x: <> Int, y: <> Int) <Abort> Int {
          if eq(y, 0) then
            abort()
          else
            div(x, y)
        }

        let main = match handle hAbort {
          let x = 6
          let y = 0
          safediv(x, y)
        } {
          Nothing() => "cannot divide by zero"
        }
      }
    |]
      `shouldEvalTo` String "cannot divide by zero"

  it "can elaborate and handle exceptions" $ do
    [r|
      mod main {
        import std

        type Maybe {
          Just(<> a)
          Nothing()
        }

        effect Abort {
          abort() ()
        }

        effect Exc! {
          catch!(a) a
          throw!() b
        }

        let h = handler {
          return(x) { Just(x) }
          abort() { Nothing() }
        }

        let e = elaboration Exc! -> <Abort> {
          catch!(e) {
            handle h e
          }
          throw!() {
            abort()
          }
        }

        let safediv = fn(x: <> Int, y: <> Int) <Exc!> Int {
          if eq(y, 0) then
            throw!()
          else
            div(x, y)
        }

        let main = handle h {
          elab[e] {
            match catch!(safediv(5, 0)) {
              Nothing() => "cannot divide by 0"
            }
          }
        }
      }
    |]
      `shouldEvalTo` Data "Maybe" "Just" [Val $ String "cannot divide by 0"]

  it "can do a state effect" $ do
    [r|
      mod main {
        import std

        effect State {
          get() Int
          put(Int) ()
        }

        let h = handler {
          return(x) {
            fn(n: <> Int) <> a { x }
          }
          get() {
            fn(n: <> Int) <> a { 
              let f = resume(n)
              f(n)
            }
          }
          put(x) {
            fn(n: <> Int) <> a { 
              let f = resume(())
              f(x)
            } 
          }
        }

        let main = {
          let f = handle h {
            let x = get()
            let _ = put(6)
            let y = get()
          }
          f(5)
        }
      }
    |]
      `shouldEvalTo` String "5, 6"

  it "can do some funky stuff" $ do
    [r|
      mod main {
        import std
        let hVal = handler {
          return(x) { x }
          val(f) {
            resume(fn () <> Int { f(5) })
          }
        }
        let timesTwo = fn(x: <> Int) <> Int {
          mul(2, x)
        }
        let main = handle hVal {
          let f = val(timesTwo)
          f()
        }
      }
    |] `shouldEvalTo` Int 10
  
  it "can do the reader effect" $ do
    [r|
      mod main {
        import std

        effect Reader! {
          ask!() a
          # TODO fix function type
          local!(f, b) b
        }

        effect Reader {
          ask() a
        }

        let h = fn(x: <> a) <> c {
          handler {
            return(y) { y }
            ask() { resume(x) }
          }
        }

        let eReader = elaboration Reader! -> <Reader> {
          ask!() { ask() }
          local!(f, c) {
            handle h(f(ask())) { c }
          }
        }

        type Triple {
          t(<> Int, <> Int, <> Int)
        }

        let double = fn(x: <> Int) <> Int {
          mul(2, x)
        }

        let main = {
          handle h(1) elab[eReader] {
            let x = ask!()
            local!(double, {
              let y = ask!()
              local!(double, {
                let z = ask!()
                t(x, y, z)
              })
            })
          }
        }
      }
    |]
      `shouldEvalTo` Data "Triple" "t" [Val $ Int 1, Val $ Int 2, Val $ Int 4]
  
  it "readsimple" $ do
    [r|
    mod main {
        import std

        let h = fn(x: <> a) <> c {
          handler {
            return(y) { y }
            ask() { resume(x) }
          }
        }

        let eReader = elaboration Reader! -> <Reader> {
          ask!() { ask() }
          local!(f, c) {
            handle h(f(ask())) { c }
          }
        }

        let double = fn(x: <> Int) <> Int {
          mul(2, x)
        }

        let main = {
          handle h(1) elab[eReader] {
            local!(double, {
              ask!()
            })
          }
        }
      }
    |] `shouldEvalTo` Int 2

  it "can do the log effect" $ do
    [r|
      mod main {
        import std

        effect Log! {
          context!(String, a) a
          log!(String) ()
        }

        effect Writer {
          write(String) ()
        }

        effect Reader {
          ask() String
        }

        let hWrite = handler {
          return(x) { "" }
          log(x) {
            let rest = resume(())
            let line = concat(x, "\n")
            concat(line, rest)
          }
        }

        # TODO handler type
        let hRead = fn(x: <> String) <> h {
          handler {
            return(y) { y }
            ask() { resume(x) }
          }
        }

        let eLog = elaboration Log! -> <Writer,Reader> {
          context!(s, c) {
            let newCtx = concat(concat(ask(), ":"), s)
            handle hRead(newCtx) c
          }
          log!(s) { 
            let msg = concat(concat(ask(), ": "), s)
            log(msg)
          }
        }

        let main = handle hRead("root") handle hWrite elab[eLog] {
          let _ = log!("one")
          context!("foo", {
            let _ = log!("two")
            context!("bar", {
              log!("three")
            })
          })
        }
      }
    |]
      `shouldEvalTo` String "root: one\nroot:foo: two\nroot:foo:bar: three\n"

