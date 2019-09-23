module Main where
import           Approx
import           Data.Complex
import           Data.Ratio
import           Hypergeom
import           Test.Tasty       (defaultMain, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)
main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ testCase "a 2F1 value" $ do
      let alpha = 2 :: Double
      h <- hypergeom 10 2 [1,2] [3] [0.2, 0.5]
      assertEqual ""
        (approx 8 1.79412894456143)
        (approx 8 h),

    testCase "a complex 2F1 value" $ do
      let c = 2 :+ 3 :: Complex Double
      h <- hypergeom 10 2 [1,2] [c] [0.2 :+ 1, 0.5]
      assertEqual ""
        (approx' 6 (1.887753 :+ 0.566665))
        (approx' 6 h),

    testCase "compare with rational" $ do
      h1 <- hypergeom 10 2 [1%2, 3] [3%2, 1%3, 2] [1%5, 1%4, 1%8]
      let h1' = fromRational h1
      h2 <- hypergeom 10 (2::Double) [1/2, 3] [3/2, 1/3, 2] [1/5, 1/4, 1/8]
      assertEqual ""
        (approx 15 h1')
        (approx 15 h2)
  ]
