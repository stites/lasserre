{-# LANGUAGE FlexibleContexts #-}
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
import Math.Algebra.Hspray.Lasserre
import Math.Algebra.Hspray.Utils
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
  let simplex = mkSimplex [v1, v2, v3, v4]

  let action = Log.cmap fmtMessage logTextStdout
  o <- usingLoggerT action $ integrate poly simplex
  print o

--  5
--  4       x
--  3
--  2    x  x
--  1 x
--  - 1  2  3  4  5
