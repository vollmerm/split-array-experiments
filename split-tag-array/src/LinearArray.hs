{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs       #-}
{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module LinearArray where
import Prelude.Linear
import qualified Data.Array as A
import qualified Data.Array.IO as MA
import Data.Array.Unsafe
import System.IO.Unsafe
import Unsafe.Linear
import Control.Monad

newtype Array  a = Array  (A.Array    Int a)
newtype MArray a = MArray (MA.IOArray Int a)

instance Show a => Show (Array a) where
  show (Array arr) = show arr

newMArray :: Int -> (MArray a %1-> Ur b) %1-> Ur b
newMArray i f = f (MArray (unsafePerformIO (MA.newArray_ (0,i))))

length :: MArray a %1-> (MArray a, Ur Int)
length = coerce length'
  where
    length' :: MArray a -> (MArray a, Ur Int)
    length' (MArray arr) =
      (MArray arr, Ur (snd (unsafePerformIO (MA.getBounds arr))))

{-# NOINLINE write #-}
write :: MArray a %1-> (Int,a) %1-> MArray a
write = coerce write'
  where
    write' :: MArray a -> (Int,a) -> MArray a
    write' (MArray arr) (i,v) =
      MArray $ unsafePerformIO (do _ <- MA.writeArray arr i v; return arr)

read :: MArray a %1-> Int -> (MArray a, Ur a)
read = coerce read'
  where
    read' :: MArray a -> Int -> (MArray a, Ur a)
    read' (MArray arr) i =
      unsafePerformIO (do e <- MA.readArray arr i; return (MArray arr, Ur e))

freeze :: MArray a %1-> Ur (Array a)
freeze = coerce freeze'
  where
    freeze' :: MArray a -> Ur (Array a)
    freeze' (MArray arr) =
      unsafePerformIO (do arr' <- unsafeFreeze arr; return (Ur (Array arr')))

index :: Array a -> Int -> a
index (Array arr) i = arr A.! i
