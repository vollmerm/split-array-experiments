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

sortArray :: Ord t => Lin.MArray t %1-> Lin.MArray t
sortArray marr = rootSubArray $ mergeSort $ newSubArray marr

mergeSort :: forall r p t . Ord t => SubArray r p t %1-> SubArray r p t
mergeSort arr = f $ A.length arr
  where
    f :: (SubArray r p t, Ur Int) %1-> SubArray r p t
    f (arr', Ur len) =
      if len <= 2
      then merge arr'
      else withSplitArray arr'
           (\left right promote ->
               promote $ merge $ combineArray (mergeSort left) (mergeSort right))

merge :: forall r p t . Ord t => SubArray r p t %1-> SubArray r p t
merge arr = f $ A.length arr
  where
    f :: (SubArray r p t, Ur Int) %1-> SubArray r p t
    f (arr', Ur len) =
      if len <= 1
      then arr'
      else loop arr' (Ur 0) (Ur (len-1)) (Ur (nextGap len))

    loop :: SubArray r p t %1-> Ur Int %1-> Ur Int %1-> Ur Int %1-> SubArray r p t
    loop larr (Ur start) (Ur end) (Ur gap) =
      if gap <= 0
      then larr
      else loop (innerLoop larr (Ur start) (Ur end) (Ur gap) (Ur start))
                (Ur start) (Ur end) (Ur (nextGap gap))

    innerLoop :: SubArray r p t %1-> Ur Int %1-> Ur Int %1-> Ur Int %1-> Ur Int %1-> SubArray r p t
    innerLoop larr (Ur start) (Ur end) (Ur gap) (Ur i) =
      if (i+gap <= end)
      then innerLoop ((\(larr', Ur b) ->
              if b
              then swap larr' (Ur i) (Ur (i+gap))
              else larr') (comp larr (Ur i) (Ur (i+gap))))
           (Ur start) (Ur end) (Ur gap) (Ur (i+1))
      else larr

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

    comp :: SubArray r p t %1-> Ur Int %1-> Ur Int %1-> (SubArray r p t, Ur Bool)
    comp arr0 (Ur i) (Ur j) =
      (\(arr', Ur vi) ->
         (\(arr'', Ur vj) ->
            (arr'', Ur (vi > vj))
         ) (A.read arr' j)
      ) (A.read arr0 i)
