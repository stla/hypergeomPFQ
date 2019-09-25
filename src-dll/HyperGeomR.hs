{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module HyperGeomR where
import           Control.Memory.Region (V)
import           Data.Vector.SEXP      (fromSEXP, toSEXP, singleton, toList)
import qualified Data.Vector.SEXP      as VS
import           Foreign.R             (SEXP)
import qualified Foreign.R             as R
import           Hypergeom
import           HypergeomU

foreign export ccall hypergeomR :: SEXP s 'R.Int -> SEXP s 'R.Real
                                -> SEXP s 'R.Real -> SEXP s 'R.Real
                                -> SEXP s 'R.Real -> IO (SEXP V 'R.Real)
hypergeomR :: SEXP s 'R.Int -> SEXP s 'R.Real -> SEXP s 'R.Real
           -> SEXP s 'R.Real -> SEXP s 'R.Real -> IO (SEXP V 'R.Real)
hypergeomR m a b x alpha = do
  let m' = fromIntegral $ VS.head $ fromSEXP m :: Int
      a' = toList $ fromSEXP a
      b' = toList $ fromSEXP b
      x' = toList $ fromSEXP x
      alpha' = VS.head $ fromSEXP alpha
  h <- hypergeom m' alpha' a' b' x'
  return $ toSEXP $ singleton h

foreign export ccall hypergeomR2 :: SEXP s 'R.Int -> SEXP s 'R.Real
                                 -> SEXP s 'R.Real -> SEXP s 'R.Real
                                 -> SEXP s 'R.Real -> IO (SEXP V 'R.Real)
hypergeomR2 :: SEXP s 'R.Int -> SEXP s 'R.Real -> SEXP s 'R.Real
            -> SEXP s 'R.Real -> SEXP s 'R.Real -> IO (SEXP V 'R.Real)
hypergeomR2 m a b x alpha = do
  let m' = fromIntegral $ VS.head $ fromSEXP m :: Int
      a' = toList $ fromSEXP a
      b' = toList $ fromSEXP b
      x' = toList $ fromSEXP x
      alpha' = VS.head $ fromSEXP alpha
  h <- hypergeomU m' alpha' a' b' x'
  return $ toSEXP $ singleton h
