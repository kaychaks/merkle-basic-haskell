{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Bits (Bits (shiftR), (.&.), (.|.))
import Data.Foldable (Foldable (foldr'), foldl')
import Data.Hashable (Hashable, hash)
import Data.Kind (Type)
import Data.List (elemIndex, findIndex)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup (sconcat))
import qualified GHC.Generics as NE

type Hash = Int

nextHighestPowerOfTwoSmallerThan :: (Num a, Bits a) => a -> a
nextHighestPowerOfTwoSmallerThan n = (1 + foldl' go (n -1) [1, 2, 4, 8, 16, 32]) `shiftR` 1
  where
    go m i = m .|. m `shiftR` i

isPower2 :: (Integral i, Bits i) => i -> Bool
isPower2 n = n .&. (n - 1) == 0

splitOnPos :: [a] -> ([a], [a])
splitOnPos [] = ([], [])
splitOnPos [x] = ([x], [])
splitOnPos (x1 : x2 : xs) = (x1 : odds, x2 : evens)
  where
    (odds, evens) = splitOnPos xs

class CMerkle a where
  data HashType a :: Type
  leaf_pad :: a
  node_pad :: a
  hhash :: a -> HashType a
  eqHash :: HashType a -> HashType a -> Bool
  showHash :: HashType a -> String
  cconcat :: HashType a -> HashType a -> HashType a

  mkLeaf :: a -> HashType a
  mkLeaf x = hhash leaf_pad `cconcat` hhash x

  mkNode :: HashType a -> HashType a -> HashType a
  mkNode x y = hhash node_pad `cconcat` x `cconcat` y

  mkLeafHash :: HashType a -> HashType a
  mkLeafHash = cconcat $ hhash leaf_pad

instance CMerkle a => Semigroup (HashType a) where
  (<>) = mkNode

instance CMerkle a => Eq (HashType a) where
  (==) = eqHash

instance CMerkle a => Show (HashType a) where
  show = showHash

merkleTreeHash :: CMerkle a => a -> [a] -> HashType a
merkleTreeHash d [] = mkLeaf d
merkleTreeHash _ [x] = mkLeaf x
merkleTreeHash d xs =
  let n = length xs
      k = nextHighestPowerOfTwoSmallerThan n
      (left, right) = bimap (merkleTreeHash d) (merkleTreeHash d) $ splitAt k xs
   in mkNode left right

combineHashes :: CMerkle a => a -> Int -> HashType a -> NE.NonEmpty (HashType a) -> Maybe (HashType a)
combineHashes d old_length old ps
  | isPower2 old_length = return $ sconcat $ NE.cons old ps
  | otherwise =
    let (odds, evens) = splitOnPos $ NE.toList ps
        go xs ys = sconcat $ NE.fromList $ [sconcat xs, sconcat ys]
     in go <$> NE.nonEmpty odds <*> NE.nonEmpty evens

auditPath :: (CMerkle a, Eq a) => a -> a -> [a] -> [HashType a]
auditPath _ _ [] = []
auditPath d el xs =
  let maybe_m = elemIndex el xs
      go _ [x] = []
      go m' xs' =
        let n' = length xs'
            k' = nextHighestPowerOfTwoSmallerThan n'
            (left, right) = splitAt k' xs'
         in if m' < k' then go m' left <> [merkleTreeHash d right] else go (m' - k') right <> [merkleTreeHash d left]
   in case maybe_m of
        Nothing -> []
        Just m -> go m xs

consistencyProof :: (CMerkle a, Eq a) => a -> [a] -> [a] -> [HashType a]
consistencyProof d old new =
  let m = length old
      n = length new
      subProof m' xs True | m' == length xs && m' == m = []
      subProof m' xs False | m' == length xs = [merkleTreeHash d xs]
      subProof m' xs b =
        let n' = length xs
            k = nextHighestPowerOfTwoSmallerThan n'
            (left, right) = splitAt k xs
         in if m' <= k
              then subProof m' left b <> [merkleTreeHash d right]
              else subProof (m' - k) right False <> [merkleTreeHash d left]
   in if m <= n then subProof m new True else []

_isIncluded :: (CMerkle a, Eq a) => a -> a -> [a] -> Bool
_isIncluded d x xs = sconcat (NE.fromList $ mkLeaf x : auditPath d x xs) == merkleTreeHash d xs

_areConsistent :: (CMerkle a, Eq a) => a -> [a] -> [a] -> Bool
_areConsistent _ [] _ = False
_areConsistent d old new =
  let old_hash = merkleTreeHash d old
      new_hash = merkleTreeHash d new
      old_length = length old
   in (old_hash == new_hash) || (Just new_hash == (combineHashes d old_length old_hash =<< NE.nonEmpty (consistencyProof d old new)))

instance CMerkle String where
  data HashType String = HashType Int
  leaf_pad = "0"
  node_pad = "1"
  cconcat (HashType a) (HashType b) = HashType $ a + b
  hhash = HashType . hash
  eqHash (HashType a) (HashType b) = a == b
  showHash (HashType a) = show a

isIncluded :: String -> [String] -> Bool
isIncluded = _isIncluded ""

areConsistent :: [String] -> [String] -> Bool
areConsistent = _areConsistent ""

main :: IO ()
main = do
  let xs = fmap show [0 .. 9]
      ys = fmap show [0 .. 4]
  print $ isIncluded "4" xs
  print $ areConsistent ys xs
