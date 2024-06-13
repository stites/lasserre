import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)
import Data.Text qualified as T
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Math.Algebra.Hspray
import Math.Algebra.Hspray.Lasserre
import Test.Hspec
import Data.Function ((&))

describePoly poly test = do
  describe (prettyNumSpray poly) $ do
    test poly

shouldApprox :: Double -> (Double, Double) -> Expectation
shouldApprox res (expect, eps) = shouldSatisfy res
  $ \r -> r & subtract expect & abs & (< eps) -- make sure the result

shouldApproxEps :: Double -> Double -> Expectation
shouldApproxEps res expect = shouldApprox res (expect, 0.0000001)


main :: IO ()
main = hspec $ do
  describe "mkPoly" $ do
    it "should not change any simplex" $ do
      let tru = [ [0, 0] , [0, 1] , [1, 1]]
      let s = mkSimplex tru
      let p = decompose $ mkPoly tru

      p `shouldBe` []
  describe "integration" $ do
    let x = lone 1
    let y = lone 2
    let z = lone 3

    describePoly ((x ^**^ 4) ^+^ y ^+^ 2 *^ (x ^*^ y ^**^ 2) ^-^ 3 *^ z) $ \poly ->
      it "compiles the example tetrahedron simplex" $ do
        o <- integrate poly $ mkSimplex
            [ [1, 1, 1]
            , [2, 2, 3]
            , [3, 4, 5]
            , [3, 2, 1]
            ]
        o `shouldBe` 33.02380952380952

    describePoly (x ^*^ y) $ \poly -> do
      it "integrates in an arbitrary simplex" $ do
        o <- integrate poly $ mkSimplex
            [ [1, 1]
            , [1, 4]
            , [4, 4]
            ] -- 3x3 triangle, *2 variables
        o `shouldBe` 9
      it "compiles the example tetrahedron simplex" $ do
        -- for now, assume that this works.
        o1 <- integrate poly $ mkSimplex
            [ [0, 0]
            , [0, 1]
            , [1, 1]
            ]
        o2 <- integrate poly $ mkSimplex
            [ [0, 0]
            , [1, 1]
            , [0, 1]
            ]
        o1 `shouldApproxEps` o2
