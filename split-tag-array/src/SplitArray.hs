{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs       #-}
{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module SplitArray where
import Prelude.Linear
import LinearArray as Lin
import Unsafe.Linear

data ArrayPart
  = W -- whole
  | L -- left
  | R -- right

data SubArray region (part :: ArrayPart) ty -- region is quantifier-bound, part is ArrayPart
  = MkSubArray { parent :: SubArray region 'W ty
               , range :: (Int,Int) -- range in root array
               , root :: Lin.MArray ty
               }

newSubArray :: Lin.MArray ty %1-> SubArray r 'W ty
newSubArray arr = -- why doesn't linear haskell support let-binding yet?
  (\(arr',len) -> MkSubArray undefined (0,len) arr') (Lin.length arr)

fromSubArray :: SubArray r 'W ty %1-> Lin.MArray ty
fromSubArray = coerce root

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
unsafeSplitLeft sa = MkSubArray (coerce sa) (l,l+len) (root sa)
  where
    (l,r) = range sa
    len = ((r-l) `quot` 2) + ((r-l) `rem` 2)

unsafeSplitRight :: SubArray r p t -> SubArray r 'R t
unsafeSplitRight sa = MkSubArray (coerce sa) (r-len,r) (root sa)
  where
    (l,r) = range sa
    len = (r-l) `quot` 2

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
    write' a (i,v) =
      if i < (r-l)
      then a { root = Lin.write (root a) (l+i,v) }
      else error ("SplitArray.write: index out of bounts " ++ show i)
      where (l,r) = range a

read :: SubArray r p t %1-> Int -> (SubArray r p t, Ur t)
read = coerce read'
  where
    read' :: SubArray r p t -> Int -> (SubArray r p t, Ur t)
    read' a i =
      if i < (r-l)
      then let (root',v) = Lin.read (root a) (l+i) in (a {root = root'}, v)
      else error ("SplitArray.read: index out of bounts " ++ show i)
      where (l,r) = range a
