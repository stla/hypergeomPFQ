{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HypergeomU (hypergeomU) where
import           Control.Monad      (when)
import           Data.Array.IO      hiding (index)
import           Data.Array.Unboxed hiding (index)
import           Data.Sequence      (Seq, index, update, (!?), (|>))
import qualified Data.Sequence      as S
import           Shared

summation :: forall a. (Eq a, Fractional a, MArray IOUArray a IO)
  => [a] -> [a] -> [a] -> Seq (Maybe Int) -> Int -> a -> Int
     -> a -> Int -> Seq Int -> IOUArray (Int, Int) a -> IO a
summation a b x dico n alpha i z j kappa jarray
  = if i == n
    then
      return 0
    else do
      let lkappa = kappa `index` (S.length kappa - 1)
      let go :: Int -> a -> a -> IO a
          go kappai !z' !s
            | i == 0 && kappai > j || i > 0 && kappai > min lkappa j =
              return s
            | otherwise = do
              let kappa' = kappa |> kappai -- correct ? seems yes
                  nkappa = _nkappa dico kappa'
                  z'' = z' * _T alpha a b kappa'
                  lkappa' = S.length kappa'
              when (nkappa > 1 && (lkappa' == 1 || kappa' !? 1 == Just 0)) $ do
                entry <- readArray jarray (nkappa - 1, 1)
                let kap0m1' = fromIntegral (kappa' `index` 0 - 1)
                    newval = x !! 0 * (1 + alpha * kap0m1') * entry
                writeArray jarray (nkappa, 1) newval
              let go' :: Int -> IO ()
                  go' t
                    | t == n + 1 = return ()
                    | otherwise = do
                      _ <- jack alpha x dico 0 1 0 t kappa' jarray kappa' nkappa
                      go' (t + 1)
              _ <- go' 2
              entry' <- readArray jarray (nkappa, n)
              let s' = s + z'' * entry'
              if j > kappai && i <= n
                then do
                  s'' <-
                    summation
                      a
                      b
                      x
                      dico
                      n
                      alpha
                      (i + 1)
                      z''
                      (j - kappai)
                      kappa'
                      jarray
                  go (kappai + 1) z'' (s' + s'')
                else go (kappai + 1) z'' s'
      go 1 z 0

jack :: (Fractional a, MArray IOUArray a IO)
  => a -> [a] -> Seq (Maybe Int) -> Int -> a -> Int -> Int -> Seq Int
     -> IOUArray (Int, Int) a -> Seq Int -> Int -> IO ()
jack alpha x dico k beta c t mu jarray kappa nkappa = do
  let i0 = max k 1
      i1 = S.length (cleanPart mu) + 1
      go :: Int -> IO ()
      go i
        | i == i1 = return ()
        | otherwise
         = do
          let u = mu `index` (i - 1)
          when (S.length mu == i || u > mu `index` i) $ do
            let gamma = beta * _betaratio kappa mu i alpha
                mu' = cleanPart $ update (i-1) (u - 1) mu
                nmu = _nkappa dico mu'
            if S.length mu' >= i && u > 1  -- mu' `index` (i - 1) > 0
              then
                jack alpha x dico i gamma (c + 1) t mu' jarray kappa nkappa
              else
                when (nkappa > 1) $ do
                  entry' <- readArray jarray (nkappa, t)
                  if not (S.null mu') -- any (> 0) mu'
                    then do
                      entry <- readArray jarray (nmu, t - 1)
                      writeArray
                        jarray
                        (nkappa, t)
                        (entry' + gamma * entry * x !! (t - 1) ^ (c + 1))
                    else writeArray
                           jarray
                           (nkappa, t)
                           (entry' + gamma * x !! (t - 1) ^ (c + 1))
          go (i + 1)
  _ <- go i0
  entry1 <- readArray jarray (nkappa, t)
  if k == 0
    then
      when (nkappa > 1) $ do
        entry2 <- readArray jarray (nkappa, t - 1)
        writeArray jarray (nkappa, t) (entry1 + entry2)
    else do
      entry2 <- readArray jarray (_nkappa dico mu, t - 1)
      writeArray jarray (nkappa, t) (entry1 + beta * x !! (t - 1) ^ c * entry2)

hypergeomU ::
     forall a. (Eq a, Fractional a, IArray UArray a, MArray IOUArray a IO)
  => Int  -- truncation weight
  -> a    -- alpha parameter (usually 2)
  -> [a]  -- "upper" parameters
  -> [a]  -- "lower" parameters
  -> [a]  -- variables (the eigen values)
  -> IO a
hypergeomU m alpha a b x = do
  let n = length x
  if all (== head x) x
    then
      return $ hypergeoI m alpha a b n (head x)
    else do
      let pmn = _P m n
          dico = _dico pmn m
          xrange = [1 .. n]
          line1 = zipWith (\i u -> ((1, i), u)) xrange (scanl1 (+) x)
          otherlines = concatMap (\j -> [((j, i), 0) | i <- xrange]) [2 .. pmn]
          arr0 =
            array ((1, 1), (pmn, n)) (line1 ++ otherlines) :: UArray (Int, Int) a
      jarray <- thaw arr0
      s <- summation a b x dico n alpha 0 1 m S.empty jarray
      return $ s + 1
