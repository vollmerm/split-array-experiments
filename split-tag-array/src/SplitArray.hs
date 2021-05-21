{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs       #-}
{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module SplitArray where
import Prelude.Linear
import LinearArray as Lin
import Unsafe.Linear
import Data.Proxy

data ArrayPart
  = W -- whole
  | L -- left
  | R -- right

data SubArray region (part :: ArrayPart) ty -- region is quantifier-bound, part is ArrayPart
  = MkSubArray { parent :: forall r q . SubArray r q ty
               , range :: (Int,Int) -- range in root array
               , root :: Lin.MArray ty
               }

newSubArray :: Proxy r -> Lin.MArray ty %1-> SubArray r 'W ty
newSubArray _ arr = -- why doesn't linear haskell support let-binding yet?
  (\(arr',len) -> MkSubArray undefined (0,len) arr') (Lin.length arr)

combineArray
    :: forall q a. -- only combine arrays in same region
       SubArray q 'L a -- left sub-array
  %1-> SubArray q 'R a -- right sub-array
  %1-> SubArray q 'W a -- combined result
combineArray = coerce combineArray'
  where
    combineArray' :: SubArray q 'L a -> SubArray q 'R a -> SubArray q 'W a
    combineArray' arr1 _arr2 = parent arr1

withSplitArray
    ::  forall q p a .
        SubArray q p a
   %1-> (forall x . SubArray x 'L a -- left sub-array
               %1-> SubArray x 'R a -- right sub-array
               %1-> (SubArray x 'W a %1-> SubArray q p a)
               %1-> SubArray q p a) -- combined result
   %1-> SubArray q p a
withSplitArray arr f =
  (\(left,right,fun) -> f left right fun)
  ((coerce (\arr' -> (unsafeSplitLeft arr',unsafeSplitRight arr',(\_ -> arr'))))  arr)

unsafeSplitLeft :: SubArray r p t -> SubArray r 'L t
unsafeSplitLeft = undefined

unsafeSplitRight :: SubArray r p t -> SubArray r 'R t
unsafeSplitRight = undefined

length :: SubArray r p t %1-> (SubArray r p t, Int)
length = coerce length'
  where
    length' :: SubArray r p t -> (SubArray r p t, Int)
    length' a = let (begin,end) = range a
                in (a,end-begin)

write :: SubArray r p t %1-> (Int,t) %1-> SubArray r p t
write = coerce write'
  where
    write' :: SubArray r p t -> (Int,t) -> SubArray r p t
    write' a (i,v) = undefined

read :: SubArray r p t %1-> Int -> (SubArray r p t, Ur t)
read = coerce read'
  where
    read' :: SubArray r p t -> Int -> (SubArray r p t, Ur t)
    read' a i = undefined
