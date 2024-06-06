# directVSmonad

_An exploration of direct and monadic patterns for solving nontrivial
problems._

## What is this about?

There are a variety of strategies for handling control flow tasks in
Scala.  Unfortunately, many examples are so simplistic that they don't
really showcase the challenges and advantages one faces in a realistic,
nontrivial scenario.

This repository is a place to explore more hefty challenges for different
styles of flow control and abstraction.  There's no "a monad is like
a burrito" here; we'll assume you have at least a basic familiarity with
the styles and simply want to see reasonable solutions to
not-straightforward problems.

## How is this organized?

Each folder contains a different challenge.  Within that folder, there is
a single `ChallengeWhatever.scala` file that contains the challenge as
a library, and a set of `SolutionSoAndSo.scala` files that each contain
a different way to solve the problem, which are intended to be run
with scala-cli using

```scala
scala-cli SolutionSoAndSo.scala ChallengeWhatever.scala
```

(plus JVM arguments if necessary, since using jvm doesn't always work).
