module Examples (testAllExamples) where

import qualified Data.Map (fromList)
import Elaine.AST (Value (..))
import Elaine.Exec (execRun, pack', Result (..))
import System.Directory (listDirectory)
import Test.Hspec
  ( Expectation,
    SpecWith,
    describe,
    expectationFailure,
    hspec,
    it,
    shouldBe, shouldSatisfy,
    runIO
  )

expected :: String -> Value
expected "state.elaine" = Int 6
expected _ = error "Example does not have an expected value"

testAllExamples :: SpecWith ()
testAllExamples = describe "Text Examples" $ do
  examples <- runIO $ listDirectory "examples"

  -- We ignore examples starting with an underscore
  let examples' = filter (\s -> head s /= '_') examples

  mapM_ testExample examples'

run :: String -> String -> Result Value
run path = execRun . pack' . (,) path

testExample :: FilePath -> SpecWith ()
testExample p = do
  contents <- runIO $ readFile $ "examples/" ++ p
  it p $ do
    run p contents `shouldBe` Right (expected p)
