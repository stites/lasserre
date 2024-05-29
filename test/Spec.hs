import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)
import Data.Text qualified as T
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Math.Algebra.Hspray
import Math.Algebra.Hspray.Lasserre
import Test.Hspec (before, describe, hspec, it, shouldBe)
import Test.Hspec.Hedgehog (
  PropertyT,
  diff,
  forAll,
  hedgehog,
  (/==),
  (===),
 )

describePoly poly test = do
  describe (prettyNumSpray poly) $ do
    test poly

main :: IO ()
main = hspec $ do
  describe "integration" $ do
    let x = lone 1
    let y = lone 2
    let z = lone 3

    describePoly (x ^*^ y) $ \poly ->
      it "compiles the example tetrahedron simplex" $ do
        o <- integrate poly $ mkSimplex
            [ [1, 1]
            , [1, 4]
            , [4, 4]
            ] -- 3x3 triangle, *2 variables
        o `shouldBe` 9

    describePoly ((x ^**^ 4) ^+^ y ^+^ 2 *^ (x ^*^ y ^**^ 2) ^-^ 3 *^ z) $ \poly ->
      it "compiles the example tetrahedron simplex" $ do
        o <- integrate poly $ mkSimplex
            [ [1, 1, 1]
            , [2, 2, 3]
            , [3, 4, 5]
            , [3, 2, 1]
            ]
        o `shouldBe` 33.02380952380952
