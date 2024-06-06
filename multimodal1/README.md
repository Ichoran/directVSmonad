## multimodal1 challenge

In this challenge, we want to interweave concurrency, error handling,
and execution of lambdas.  These kinds of patterns occur naturally when
handling networking, databases, file IO and the like, but here we'll
only provide a simple interface to add to an `AtomicInteger`.

### Data Types

This challenge involves two input datatypes, one function datatype,
and one output datatype.

The first input type is `Iffy`, which models a computation that might
fail with an exception.  Solutions are encouraged to extend `Iffy` to
wrap the failure in their preferred way--this shouldn't count against
the solution, because if one adopted the solution wholeheartedly, `Iffy`
would express errors that way by default.  It has a single method, `get`,
which can return an `Int` or fail with an exception.

The second input type is `Slow`, which models a computation that takes
a while and may block.  It has a fixed value that it will return, but
it will block for ten times its value in milliseconds (by calling
`Thread.sleep` and checking progress with `System.nanotime`) before
it gives that value.

The function datatype is `Magic`.  `Magic` contains a secret value
and a custom operation; you supply its `Invoke` method with a function
that is given the secret value and whose (`Int`) output is then applied
to the custom operation.

The output datatype is `Rec`.  You can `put` `Int` values into a `Rec`
and it will sum them.  `Rec` needs to be closed when you're done using
it; the sum can be observed with `get`.  Operations on `Rec` are threadsafe,
guarded by `synchronized` blocks.

### The challenge type signature

The challenge is expressed as `(() => Res, Seq[(Slow, Iffy, Magic)]) => Option[(Int,
Int)]`, plus correct mutable state written into the companion objects of
`Res` and `Slow`.

### The challenge values

If any `get` call to an `Iffy` fails, the entire challenge function should
return `None`.  Otherwise, the function should return a `Some[(Int, Int)]`.

The value of the first `Int` should be equal to the sum of all the
non-negative secret values in the `Magic` objects.  That is, if the
input `Seq` is `input`, it should equal

```scala
var sum = 0
input.foreach{ case (_, _, magic) =>
  magic.invoke{ i =>
    if i >= 0 then
      sum += i
    0
  }
}
sum
```

However, for every item, one must create a `Res` and `put` into it both
the magic value and the result of `magic.invoke` on the `Slow` value for
that index.

The value of the second `Int` should be equal to the sum of: the `Iffy` and
`Slow` values referenced by index from the secret value of each `Magic` if
the `Slow` value is greater than the secret value, or just the `Slow` value
if it's less or equal.  Negative secret values should be ignored as indices.  This sum
must also be written into a single `Res` that should be closed before the
function returns.  It is fine to read the result out of the `Res`, or to
compute it separately.  The value should be equal to

```scala
var sum = 0
input.foreach{ case (_, _, magic) =>
  magic.invoke{ i =>
    if i >= 0 then
      val (slow, iffy, _) = input(i)
      sum += slow.get match
        case v if v > i => v + iffy.get
        case v => v
  }
}
sum
```

### The challenge exit state

On either success or failure, all `Res` values must be closed.

On success, `1 + input.length` resources must have
been created, and the appropriate values must have been written into
them.

On success, the entire operation must complete within 50 milliseconds
plus 1.5 * the longest `Slow` sleep time.

On failure, the entire operation must complete within 50 milliseconds
plus 1.5 * the shortest `Slow` sleep time corresponding to a failing
(but needed) `Iffy`.

**Good luck!**

