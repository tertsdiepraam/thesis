import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Effen

import qualified Data.Map as Map

example1 :: Comp
example1 = App (Fun "x" (If (Var "x") (return' False) (return' True))) (BoolV True)

example2 :: Comp
example2 = Do "x" (app' plus' (5::Int)) (app' (Var "x") (6::Int))

example3 :: Comp
example3 =
  Handle (Han (Handler
    ("x", Return $ Pair (Var "x") (StringV ""))
    (Map.fromList [
      ("write", OpHandle "s" "k" $
        Do "pair" $ app' (Var "k") ()           $
        Do "x"    $ app' fst' (Var "pair")      $
        Do "acc"  $ app' snd' (Var "pair")      $
        Do "s'"   $ app' concat' (Var "s")      $
        Do "s''"  $ app' (Var "s'") (Var "acc") $
        Return    $ Pair (Var "x") (Var "acc")
      )
    ])
  ))
  (Do "_" (op' "write" "hello")
  (Do "_" (op' "write" "world")
    (Return Unit)
  ))

testEval :: Test.HUnit.Test
testEval = TestList $ map mkEvalTest
  [
    ("Not", BoolV False, example1),
    ("BuiltInPlus", IntV 11, example2),
    ("Write", Pair Unit (StringV "helloworld"), example3)
  ]
  where
    mkEvalTest (name, val, comp) = 
      TestLabel name $ TestCase $ assertEqual
        (name ++ " failed")
        val (eval comp)

testSubstitute :: Test.HUnit.Test
testSubstitute = TestList $ map mkSubstituteTest
  [
    ("x", BoolV False, 
      Return (Var "x"),
      Return (BoolV False)),
    ("x", BoolV True,
      If (Var "x") (Return (BoolV True)) (Return (BoolV False)),
      If (BoolV True) (Return (BoolV True)) (Return (BoolV False)))
  ]
  where
    mkSubstituteTest (var, val, comp, exp) = TestCase $ assertEqual "" exp (substitute1 var val comp)

main :: IO ()
main = defaultMain $ hUnitTestToTests $ TestList
  [
    testEval,
    testSubstitute
  ]
