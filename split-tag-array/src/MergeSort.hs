{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs       #-}
{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module MergeSort where

import SplitArray as A
import LinearArray as Lin
import Prelude.Linear

sortArray :: Ord t => Lin.MArray t -> Lin.MArray t
sortArray marr = fromSubArray $ mergeSort $ newSubArray marr

mergeSort :: forall r p t . Ord t => SubArray r p t %1-> SubArray r p t
mergeSort arr = f $ A.length arr
  where
    f :: (SubArray r p t, Ur Int) %1-> SubArray r p t
    f (arr', Ur len) =
      if len < 2
      then arr'
      else  merge $ withSplitArray arr'
                        (\left right promote ->
                           promote $ combineArray (mergeSort left) (mergeSort right))

merge :: forall r p t . Ord t => SubArray r p t %1-> SubArray r p t
merge arr = f $ A.length arr
  where
    f :: (SubArray r p t, Ur Int) %1-> SubArray r p t
    f (arr', Ur len) =
      if len < 2
      then arr'
      else loop arr' (Ur 0) (Ur (len-1)) (Ur len)

    loop :: SubArray r p t %1-> Ur Int %1-> Ur Int %1-> Ur Int %1-> SubArray r p t
    loop = undefined

    nextGap :: Int -> Int
    nextGap gap = if gap <= 1 then 0 else (gap `quot` 2) + (gap `rem` 2)

    swap :: SubArray r p t %1-> Ur Int %1-> Ur Int %1-> SubArray r p t
    swap arr0 (Ur i) (Ur j) =
      (\(arr', Ur vi) ->
         (\(arr'', Ur vj) ->
            (\arr''' ->
               (A.write arr''' (i,vj))
            ) (A.write arr'' (j,vi))
         ) (A.read arr' j)
      ) (A.read arr0 i)

