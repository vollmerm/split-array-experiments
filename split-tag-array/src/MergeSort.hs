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

mergeSort :: Ord t => SubArray r p t %1-> SubArray r p t
mergeSort arr =
  (\(arr',len) ->
     if len < 2
     then arr'
     else  merge $ withSplitArray arr'
                        (\left right promote ->
                           promote $ combineArray (mergeSort left) (mergeSort right)))
  (A.length arr)

merge :: Ord t => SubArray r p t %1-> SubArray r p t
merge = undefined
