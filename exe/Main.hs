{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Colog qualified as Log
import Control.Arrow ((&&&))
import Control.Exception (assert)
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable as F
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Math.Algebra.Hspray
import Numeric.LinearAlgebra hiding ((!!), (<>))
import Numeric.LinearAlgebra.Data qualified as LA (fromLists)
import Numeric.SpecFunctions (factorial)
import Text.Show.Pretty

import Colog (
  Message,
  WithLog,
  fmtMessage,
  logDebug,
  logInfo,
  logTextStdout,
  logWarning,
  usingLoggerT,
 )

-- factorial which crashes if the input is >170 (which cannot be represented by a 64bit float)
factorial' :: Int -> Double
factorial' n = assert (n < 170) $ factorial n

printPolyMat :: (MonadIO m) => Spray R -> m ()
printPolyMat s = forM_ (zip (F.toList $ allExponents s) (allCoefficients s)) $ \(e, coeff) -> do
  liftIO $ print (F.toList e <> L.replicate (n - length e) 0, coeff)
  where
    n :: Int
    n = numberOfVariables s

-- dev version
integratePolynomialOnSimplex :: forall env m. (WithLog env Message m, MonadIO m) => Spray Double -> [[R]] -> m Double
integratePolynomialOnSimplex p s =
  assert (all ((n ==) . length) s) $ do
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
                let coef' = coef * (product $ map factorial' $ F.toList powers)
                pure $ s + coef' / (product [n' + 1 .. n' + (fromIntegral d :: Double)])
        )
        0
        (zip qexponents qcoeffs)
    pure $ abs (det b) / factorial' n * s
  where
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
    v = vector (last s)

    b :: Matrix R
    b = tr (LA.fromLists (init s) - asRow v)

    gens :: [Spray R]
    gens = map lone [1 .. n]

    newvarIx :: [Int] -- just an intermediate thing
    newvarIx = [0 .. fromIntegral n - 1]

main :: IO ()
main = do
  -- variables
  let x = lone 1 -- (1, 3)
  let y = lone 2 -- (2, 3)
  let z = lone 3 -- (3, 3)

  -- polynomial
  let poly :: Spray Double =
        (x ^**^ 4)
          ^+^ y
          ^+^ 2 *^ (x ^*^ y ^**^ 2)
          ^-^ 3 *^ z
  putStrLn $ prettyNumSpray poly

  -- simplex (tetrahedron) vertices
  let v1 = [1, 1, 1]
  let v2 = [2, 2, 3]
  let v3 = [3, 4, 5]
  let v4 = [3, 2, 1]
  let simplex = [v1, v2, v3, v4]

  let action = Log.cmap fmtMessage logTextStdout
  o <- usingLoggerT action $ integratePolynomialOnSimplex poly simplex
  print o
