# TODO

## Concrete tasks

- [x] Create repo
- [x] Update notes document with newer Linear Haskell syntax
- [ ] Flesh out `SubArray` example a little more
- [ ] Fill in `TODO` sections of fractional permissions section
- [ ] Add some examples with Granule syntax and `[Unique]` types
- [ ] Add section on fusion of unique arrays

### Array stuff in Linear Haskell

 - [x] Implement a simple version of `SubArray` type from the notes.
 - [x] Implement `withSubArray` (splitting and ST-style tracking)
 - [x] Implement some simple functions
   - [x] Merge sort
   - [ ] ...
 - [ ] Implement a split/merge strategy not using lexical scoping trick
 - [ ] (strecth) For comparison, implement a purely dynamically enforced version (run-time tag values)

### Array stuff in Granule

 - [x] Implement simple mutable arrays
 - [ ] Implement a generic array type
 - [ ] Fusion: push (and maybe pull) arrays 
 - [ ] Implement sub-arrays and splitting
 
### Parallel stuff in Granule

 - [ ] Stub out simple parallel combinators in Granule
 - [ ] (I guess) Implement the parallel combinators in the interpreter
 - [ ] (eventually) Use fractional permissions to share (temporarily) immutable arrays between threads

## Things to explore

 * Look into `reflection` package, and see about coming up with a way to more cleanly do type-level tags/ids for array views. For example, defining a `KnownSplit s => ...` or something. Look at `linear` library and `KnownV` to start.
 * Sketch out an interface that's more general than just arrays (splitting mutable sets, etc). There are other kinds of mutable data we want to consume/mutate in parallel, right?

## Misc

 * Compare to Ed Kmett's proposals: https://github.com/tweag/linear-base/issues/312
 * Look at reflection paper: http://okmij.org/ftp/Haskell/tr-15-04.pdf
