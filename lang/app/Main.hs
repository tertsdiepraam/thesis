module Main where

import Effen
import qualified Data.Map as Map

example3 :: Comp
example3 =
  Handle (Han (Handler
    ("x", Return $ Pair (Var "x") (StringV ""))
    (Map.fromList [
      ("write", OpHandle "s" "k" $
        Do "pair" (app' (Var "k") ()          ) $
        Do "x"    (app' fst' (Var "pair")     ) $
        Do "acc"  (app' snd' (Var "pair")     ) $
        Do "s'"   (app' concat' (Var "s")     ) $
        Do "s''"  (app' (Var "s'") (Var "acc")) $
          Return $ Pair (Var "x") (Var "s''")
      )
    ])
  ))
  (
    Do "_" (op' "write" "hello") $
    Do "_" (op' "write" "world") $
      Return Unit
  )

main :: IO ()
main = do
  eval' example3
  pure ()
