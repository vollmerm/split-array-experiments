# split-tag-array

Demo of using Linear Haskell to split and combine mutable arrays using type-level tags.

``` haskell
combineArray
    :: forall q a. -- only combine arrays in same region
       SubArray q 'L a -- left sub-array
  %1-> SubArray q 'R a -- right sub-array
  %1-> SubArray q 'W a -- combined result

withSplitArray
    ::  forall q p a .
        SubArray q p a -- arbitrary split array
   %1-> (forall x . SubArray x 'L a -- left sub-array
               %1-> SubArray x 'R a -- right sub-array
               %1-> (SubArray x 'W a %1-> SubArray q p a)
               %1-> SubArray q p a) -- combined result
   %1-> SubArray q p a -- same region/part as input split array
```

See `./src/MergeSort.hs` for an example use of the interface. Note that the implementation of merge sort doesn't require any coercions or unsafe operations!
