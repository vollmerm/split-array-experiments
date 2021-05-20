# Deterministic Parallel Programming with Graded Modal Types

I wrote that title because it sounds cool but I don't know if it's actually appropriate. This is a brainstorming document for a few related ideas about parallel programming with linear and modal types.

The structure of this document so far is:

 1. some exploration of "pure" parallel programming with mutable data using linear types, and
 2. the beginnings of an idea for adapting prior work on DRF programming using "fractional permissions" to graded modal types

There's a bit of a narrative between the two relating to how we can split up "independent" work. First, according to the structure of data (writing to distinct parts of some mutable data), and second according to control flow (classifying data as writable or read-only at different points of a program's execution).

## Divide-and-conquer with mutable linear data

The lvish PLDI paper ("Taming the Parallel Effect Zoo") has an example of using a parallel ST monad to mutate disjoint parts of an array in parallel, but it always struck me as awkward and limited. Here's the example:

```haskell
runParVecT 10 (
  do set "a" -- fill all 10 slots with "a"
     ptr <- reify -- get pointer to state
     new <- pickLetter ptr -- call pre-existing ST code
     forkSTSplit (SplitAt 5) -- split the vector at index 5
       (write 0 new) -- left child computation
       (write 0 "c") -- right child computation
     -- ptr is again accessible here
...)
```

In this example, `write 0 new` and `write 0 "c"` are two forked computations, handling the left and right portions of the 10 element vector. As the paper explains, for this to work it is important to ensure no aliases of the state escape, so it is hidden inside the state of the monad. We can use ordinary `ST` actions, but only sequentially---any parallelism needs to be introduced explicitly by monadic actions that manipulate the state, and things like writing to or splitting the state are built-in to the monad. If the shared data in your parallel application is more complicated than a simple vector, you'll end up with a good amount of boilerplate (which all has to be trusted, since you're implementing the unsafe bits now).

But what if we could use proper linear types? We already know we can use linear types to implement mutable arrays without needing to use a monad. Can we use linear types to implement shared memory parallelism in "pure" code?

Basically, the issue is as follows:

 * We can use ordinary functional programming to make independent functional work *parallel*
 * We can use linear functional programming to make functional programs work with *mutable* data
 * But when we combine these approaches, to try to do *parallel* programming on *mutable* data, it doesn't work out!

As an ongoing example, I'll use _merge sort_, and some pseudo-Haskell (not intended to actually be valid Haskell!) to demonstrate what I'm getting at.

```haskell
sort :: [Int] -> [Int]
sort xs =
  let half = (length xs)/2 
      left = take half xs
      right = drop half xs
  in merge (sort left) (sort right)
```

This is a classic example of parallel functional programming for a reason! Computing `sort left` and `sort right` are totally independent tasks. All we need to do is express that they are to execute in parallel, and we're done. This can be done in normal Parallel Haskell with `par` and `pseq` but for convenience I'll use a parallel tuple `(..||..)` like you'd find in Parallel ML and Manticore (and Gibbon).

```haskell
-- parallel merge sort with parallel tuple
psort :: [Int] -> [Int]
psort xs =
  let half = (length xs)/2 
      left = take half xs
      right = drop half xs
      (left', right') = (psort left || psort right)
  in merge (left') (right')
```

We can also write an in-place variant of merge sort on arrays (here using pseudo-Haskell with linear types). Now instead of splitting a list up, we compute a mid-point, and we pass in a range of indices to sort. Rather than re-use the `arr` variable, we have to thread the mutable array through the computation.

```haskell
-- in-place merge sort with linear types
isort :: (Int,Int) -> Arr Int %1-> Arr Int
isort (left,right) arr 
  | right > left =
  let mid = 1 + (right-1)/2
      arr' = isort (left,mid) arr
      arr'' = isort (mid+1,right) arr'
  in merge arr'' (left,mid,right)
```

Now there's a dependency between the two calls to `isort` in the function: sorting the right half of the array depends on the result of sorting the left half, but *only on paper* because we need the linear array returned from the sorting of the left half. Of course, in reality the behavior of the two recursive cases of mergesort are still totally independent, but this fact can no longer be determined by simply glancing at the syntax.

In fact, even more sophisticated independence analyses will determine that the `arr` value is shared across the two calls, because both will read and write to it. To show that the reads and writes are on distinct parts of the array would require an analysis that reasons about the indices and the arithmetic on them (which might be fun, but difficult).

So, what should we do about this? One approach might be to *split* and *combine* the array. 

If there was a way to somehow "split" `arr` into two sub-arrays or views, they could both be linear, and we'd break the dependency relation between the two `isort` calls. Once we do that, how do we get the full array back? If we could "combine" the two views back into a full array, and somehow ensure that the combining operation was only ever applied to views originating from the same array, we'd be done.

```haskell
-- parallel in-place merge sort with linear types
pisort :: Arr Int %1-> Arr Int
pisort arr =
  let (arr1,arr2) = split arr
      (arr1',arr2') = (pisort arr1 || pisort arr2)
      arr' = combine arr1' arr2'
  in merge arr'
```

There's still a problem, though. We don't have any guarantee that we've properly combined sub-arrays that originated from the same array, or that we've combined the sub-arrays in the correct order. Since `split` and `combine` don't *actually* generate new arrays, and we only actually just have the one array we're passing around views of, we can't let programmers combine sub-arrays in ways that lead to an invalid result.

So, the next question is: how do we use types to enforce a protocol for splitting and combining mutable arrays? One approach might be to use higher rank types, like with `runST`. Using existentially bound variables, we can ensure that the left and right sub-arrays can only be combined in the right way (with a provided combining function).

```haskell
data SubArr region part ty -- a 'part' is Left, Right, or Whole
combineArray
    :: forall q a. -- only combine arrays in same region
       SubArr q 'Left a -- left sub-array
   %1-> SubArr q 'Right a -- right sub-array
   %1-> SubArr q 'Whole a -- combined result
withSplitArray 
    :: forall q p a . 
       SubArr q p a
   %1-> (forall x . SubArr x 'Left a -- left sub-array
               %1-> SubArr x 'Right a -- right sub-array
               %1-> (SubArr x 'Whole a %1-> SubArr p q a)
               %1-> SubArr q p a) -- combined result
   %1-> SubArr q p a
```

The `withSplitArray` function would probably need an extra function argument or something to handle when the array is small and can't be split, but this is the general idea of it. This is not a totally new idea: _lvish_ uses tricks like this (of course, _lvish_ doesn't deal with linear types, nor with anything as thoroughly imperative as a mutable array).

Using this fancy new function, we can write a proper parallel merge sort over mutable (sub-)arrays. 

```haskell
psort :: SubArr q p a %1-> SubArr q p a
psort arr = merge $ withSubArray arr f
  where f left right promote = 
      let (a1,a2) = (psort left || psort right) 
      in promote $ combineArray a1 a2
```

All of this can obviously be applied to data types other than arrays, for problems other than merge sort, and for splitting strategies other than simply dividing a structure in half. But we're still left with something a little bit awkward, in my opinion. 

As language designers, can we formulate this in a way that doesn't require anything as complex as higher-rank types, and as something a bit more natural than the unwieldy `withSplitArray` combinator above? If we're not worried about being able to express this in Linear Haskell or whatever, and we can just design a language specifically to do this, would that be simpler/easier to understand?

Beyond that, there's perhaps a bigger problem relating to expressivity. This pattern will work nicely for traditional fork-join parallelism, but (I think) handle other parallel programming techniques like futures. The `runST` trick limits us to only using the sub-arrays inside the lexical scope of the splitting---we can't (for example) return them from the function and combine them somewhere else.

All of this so far would be doable in a theoretical version of Linear Haskell that actually worked well enough to write real programs in. Just implementing some common parallel programming patterns using this approach might make for a decent Haskell Symposium paper or workshop paper. If we had a really good implementation and evaluation (like, we built a compiler targeting C or LLVM and had benchmarks rivaling C++ performance) we could aim for PPoPP, but it'd maybe need a bit more. Since we're using linear types everywhere, if we could have the compiled parallel programs not need garbage collection, that would be a big plus for the PPoPP crowd. Also a plus if we could stage the computations to compile to specialized hardware (GPUs, FPGAs), which might not be that difficult.

To do something more interesting from a PL perspective with this, and tying it even more to the work on *graded modal types*, it's maybe worth thinking about this problem completely differently. Instead of using a type system to enforce this protocol of splitting a value and combining it in the same way, we can think of different *permissions* that we might have to operate on values. If we can use permissions to distinguish between reads and writes to the same resource, can we use permissions to distinguish between reads and writes to distinct parts of the same resource?

## Fractional permissions and independence

Permissions-based systems already resemble co-effects (question for Dominic: what exactly is the relationship? Boyland's 2003 paper calls permissions the "dual" of effects), and they give us a way to reason about reading and writing mutable data.

**TODO:** Elaborate. Inspired by old (mid-2000s) papers on permissions, and in particular fractional permissions.

To see if  `s_1` and `s_2` can be interleaved, we check if there's some partition of the permission set such that they share no permissions.

```
P_1 |- s_1
P_2 |- s_2
----------
P_1,P_2 |- s_1 || s_2
```

Additionally, permissions can be allocated and consumed, so we check statements with an input permission set and an output permission set. The following means "`s` can be evaluated with permissions `P_1` and permissions `P_2` will be available after." 

```
P_1 |- s => P_2
```

When you're doing parallel programming with mutable data, it's sometimes worthwhile to distinguish between reads and writes. Reads don't conflict with each other, but writes conflict with each other and with reads.

But it's not enough to just distinguish between "used once" (can write) and "used many" times (can read, no write). We may want a resource to be shared for one section of a program and unique for some other section of a program. So, if we've spread out a bunch of read-only aliases of a resource, how do we then later write to it?

The _fractional permissions_ approach allows us to "collect" all read permissions and use them to re-assemble a write/full permission.

```
P == (e)P, (1-e)P
```

Substructural rule, allows a permission to be split into two fractional permissions. Here `e` is some fraction between zero and one (exclusive). Writing is only allowed with `(1)P` permission. Reading is allowed at `(e)P` permission.

**TODO:** Continue explaining fractional permissions

This gives us a way to show that a parallel evaluation `(s_1 || s_2)` will not race on any data, since the two statements cannot ever simultaneously write to (or write+read) the same resource. Already this seems like something that'd be interesting to do in the context of Granule. There are various other kinds of analyses that people have done with permissions systems.

Going even further, we can think about how to work at a granularity level finer than whole resources. Can we extend this approach to reason about splitting up or otherwise using *distinct subsets* of a resource? For merge sort, we wanted to do `(sort left || sort right)` where left and right were sub-lists or sub-arrays (views). In the latter case, maybe what we want is a way to *break up write permissions for the left and right half of the array, and combine them when we're done*. So, we can split permissions on one axis for aliasing, and we can split permissions  on another axis based on the data's structure. Does that make any sense?s

**TODO:** More speculation (examples?) for permissions on shared arrays

## Related papers

Fractional Permissions for Race-Free Mutable References in a Dataflow Intermediate Language - https://dl.acm.org/doi/abs/10.1145/2957319.2957373

Taming the Parallel Effect Zoo - http://samth.github.io/effectzoo-pldi14.pdf

Checking Interference with Fractional Permissions - http://www.cs.uwm.edu/faculty/boyland/papers/permissions.pdf
