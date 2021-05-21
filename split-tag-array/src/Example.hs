{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where
import Prelude.Linear
import LinearArray
import MergeSort

mkarray :: Int -> [(Int,a)] -> Array a
mkarray size pairs = unur $ newMArray size (\ma -> freeze $ foldl write ma pairs)

testarray1 :: Array Int
testarray1 = mkarray 5 [(0,5),(1,4),(2,3),(3,2),(4,1),(5,0)]

mksortarray :: Ord a => Int -> [(Int,a)] -> Array a
mksortarray size pairs = unur $ newMArray size (\ma -> freeze $ sortArray $ foldl write ma pairs)

testarray2 :: Array Int
testarray2 = mksortarray 5 [(0,4),(1,5),(2,8),(3,1),(4,3),(5,2)]
