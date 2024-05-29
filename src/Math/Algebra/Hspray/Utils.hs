module Math.Algebra.Hspray.Utils where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable as F
import Data.List qualified as L
import Math.Algebra.Hspray

printPolyMat :: (MonadIO m) => Spray Double -> m ()
printPolyMat s = forM_ (zip (F.toList $ allExponents s) (allCoefficients s)) $ \(e, coeff) -> do
  liftIO $ print (F.toList e <> L.replicate (n - length e) 0, coeff)
  where
    n :: Int
    n = numberOfVariables s
