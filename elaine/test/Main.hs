module Main where

import Elaine.AST
import Elaine.Eval (eval0)
import Test.Hspec

tt :: Expr
tt = Val $ Bool True

ff :: Expr
ff = Val $ Bool False

main :: IO ()
main = hspec $ do
  it "gives reduces if expressions" $ do
    eval0 (If tt tt ff) `shouldBe` Bool True
    eval0 (If ff tt ff) `shouldBe` Bool False
    eval0 (If tt ff tt) `shouldBe` Bool False
    eval0 (If ff ff tt) `shouldBe` Bool True

  it "binds simple lets" $ do
    eval0 (Let "x" (Val $ Int 5) $ Var "x") `shouldBe` Int 5
    eval0 (Let "x" (Val $ Int 5) $ Var "x") `shouldBe` Int 5
    eval0 (Let "x" (Val $ Int 5) $ (Let "x" (Val $ Int 6) (Var "x"))) `shouldBe` Int 6
-- eval0 (App (Val $ Lam ["x", "y"] (If (Var "x") (Var "y") tt)) [If tt tt tt, If ff ff ff]) `shouldBe` tt