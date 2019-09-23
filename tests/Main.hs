module Main where
import           Approx
import           Hypergeom
import           Test.Tasty       (defaultMain, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)

main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ testCase "a 2F1 value" $ do
      let alpha = 2 :: Double
      h <- hypergeom 10 2 [1,2] [3] [0.2, 0.5]
      assertEqual []
        (approx 8 1.79412894456143)
        (approx 8 h)
  ]
