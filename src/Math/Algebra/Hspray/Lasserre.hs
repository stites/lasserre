{-# LANGUAGE UnicodeSyntax #-}

module Math.Algebra.Hspray.Lasserre where

import Colog qualified as Log
import Control.Arrow ((&&&))
import Control.Exception (assert)
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable as F
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.Maybe
import qualified Data.Vector as V
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Math.Algebra.Hspray
import Numeric.LinearAlgebra hiding ((!!), (<>))
import Numeric.LinearAlgebra.Data qualified as LA
import Numeric.SpecFunctions (factorial)

-- factorial will crashes if the input is >170, this cannot be represented by a 64bit float
factorial' :: Int -> Double
factorial' = factorial

newtype Simplex = Δ {unSimplex :: Matrix R}
  deriving (Show, Eq)

mkSimplex :: [[R]] -> Simplex
mkSimplex = Δ . (\m -> assert (rows m - cols m == 1) m) . LA.fromLists


newtype Poly = Polytope { unPolytope :: Matrix R}
  deriving (Show, Eq)


mkPoly :: [[R]] -> Poly
mkPoly = Polytope . (\m -> assert (rows m > cols m) m) . LA.fromLists

decompose :: Poly -> [Simplex]
decompose p = assert False $ -- incomplete. you can't just take the midpoint, that's not going to handle all triangles.
  catMaybes $ V.toList $ flip V.imap rs $ \i r ->
  if i <= (rows pM - cols pM)
  then Just . mkSimplex $ V.toList (V.slice i (cols pM) rs) <> [md]
  else Nothing
  where
    pM :: Matrix R
    pM = unPolytope p

    rs :: V.Vector [R]
    rs = V.fromList (LA.toLists pM)

    md :: [R]
    md = (/ fromIntegral (rows pM)) . sum <$> LA.toLists (tr pM)

integrateTri :: forall m. Monad m => Spray Double -> Poly -> m Double
integrateTri spray p = foldlM (\t s -> (t +) <$> integrate spray s) 0 (decompose p :: [Simplex])

-- §3.3, De Loera, Dutra & Koeppe et al. (2012-04-01) Software for Exact Integration of Polynomials over Polyhedra.
integrateCone :: forall m. Monad m => Spray Double -> Poly -> m Double
integrateCone f p = error "TODO"

integrate :: forall m. Monad m => Spray Double -> Simplex -> m Double
integrate p sim = do
    o <- reverse <$> foldlM mkNewVars [] newvarIx
    q <- foldlM (mkQ o) zeroSpray (zip pexponents pcoeffs)
    let
      qexponents :: [Exponents]
      qexponents = F.toList $ allExponents q

      qcoeffs :: [Double]
      qcoeffs = allCoefficients q
    s :: Double <-
      foldlM
        ( \s (powers, coef) -> do
            let d = sum powers
            if d == 0
              then pure $ s + coef
              else do
                let coef' = coef * product (map factorial' $ F.toList powers)
                pure $ s + coef' / product [n' + 1 .. n' + (fromIntegral d :: Double)]
        )
        0
        (zip qexponents qcoeffs)

    pure $ abs (det b) / factorial' n * s
  where
    s = unSimplex sim
    mkQ :: [Spray R] -> Spray R -> (Exponents, Double) -> m (Spray R)
    mkQ newvars' q (powers, coef) = do
      let mpower = fromMaybe 0 . (powers Seq.!?)
      let term = foldl (\term j -> term ^*^ newvars' !! j ^**^ mpower j) unitSpray [0 .. length pexponents - 1]
      pure $ q ^+^ coef *^ term

    pexponents :: [Exponents]
    pexponents = F.toList $ allExponents p

    pcoeffs :: [Double]
    pcoeffs = allCoefficients p

    mkNewVars :: [Spray R] -> Int -> m [Spray R]
    mkNewVars nvars i = do
      let newvar = constantSpray $ v `atIndex` i
      let bi = b ! i
      fin <- foldlM (mkNewVar bi) newvar newvarIx
      pure $ fin : nvars

    mkNewVar :: Vector R -> Spray R -> Int -> m (Spray R)
    mkNewVar bi newvar j = do
      let nv = newvar ^+^ (bi `atIndex` j *^ (gens !! j))
      return nv

    n :: Int
    n = numberOfVariables p

    n' :: Double
    n' = fromIntegral n

    v :: Vector R
    v = s ! (rows s - 1)

    b :: Matrix R
    b = tr ((takeRows (rows s - 1) s) - asRow v)

    gens :: [Spray R]
    gens = map lone [1 .. n]

    newvarIx :: [Int] -- just an intermediate thing
    newvarIx = [0 .. fromIntegral n - 1]
