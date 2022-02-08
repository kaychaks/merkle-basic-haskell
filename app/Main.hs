module Main where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Bits (Bits (shiftR), (.|.))
import Data.Foldable (Foldable (foldr'), foldl')
import Data.Hashable (Hashable, hash)
import qualified Data.List.NonEmpty as NE

type Hash = Int

-- this can be also rep as a Map<a, Hash>
data Merkle a = Merkle Hash a deriving (Show)

unHash :: Merkle a -> Hash
unHash (Merkle h _) = h

def :: Monoid a => [a] -> a
def = mempty

nextHighestPowerOfTwoSmallerThan :: (Num a, Bits a) => a -> a
nextHighestPowerOfTwoSmallerThan n = (1 + foldl' go (n -1) [1, 2, 4, 8, 16, 32]) `shiftR` 1
  where
    go m i = m .|. m `shiftR` i

merkleTreeHash :: (Hashable a, Monoid a) => [a] -> NE.NonEmpty (Merkle a)
merkleTreeHash xs@[] = return $ Merkle (hash $ def xs) mempty
merkleTreeHash [x] = return $ Merkle (hash x) x
merkleTreeHash xs =
  let k = nextHighestPowerOfTwoSmallerThan $ length xs
      (left, right) = bimap merkleTreeHash merkleTreeHash $ splitAt k xs
   in left <> right

getMerkle :: (Hashable a, Monoid a, Eq a) => a -> [a] -> Maybe (Merkle a)
getMerkle x = fmap NE.head . NE.nonEmpty . NE.dropWhile (\(Merkle _ a) -> a /= x) . merkleTreeHash

getHash :: (Hashable a, Monoid a, Eq a) => a -> [a] -> Maybe Hash
getHash x xs = unHash <$> getMerkle x xs

auditPath :: (Hashable a) => Int -> [a] -> Maybe [Hash]
auditPath x xs | x > length xs = Nothing
auditPath x xs = error "todo"

main :: IO ()
main = putStrLn "Hello, Haskell!"
