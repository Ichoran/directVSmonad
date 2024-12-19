//> using scala 3.3.4
//> using dep "org.typelevel::cats-effect:3.5.7"

import cats.effect.{IO, IOApp, Resource}
import cats.effect.std.Supervisor
import cats.data.Chain
import cats.syntax.all.*

import scala.concurrent.duration.*

object ChallengeMM1:
  trait Slow:
    def get: IO[Int]

  private[ChallengeMM1] object Slow:
    def make(value: Int): Slow =
      new Slow:
        override val get: IO[Int] =
          IO.sleep((value * 10).millis).as(value)
  end Slow

  trait Iffy:
    def get: IO[Int]

  private[ChallengeMM1] object Iffy:
    def make(value: Int, isBroken: Boolean): Iffy =
      new Iffy:
        override val get: IO[Int] =
          if isBroken then IO.raiseError(Exception("Broken"))
          else IO.pure(value)
  end Iffy

  trait Magic:
    def invoke(f: Int => IO[Int]): IO[Int]

  private[ChallengeMM1] object Magic:
    def make(secret: Int, op: Int => Int): Magic =
      new Magic:
        override def invoke(f: Int => IO[Int]): IO[Int] =
          f(secret).map(op)
  end Magic

  trait Res:
    def put(value: Int): IO[Unit]
    def get: IO[Int]
  end Res

  trait ResFactory:
    def createRes: Resource[IO, Res]
    private[ChallengeMM1] def currentState: IO[ResFactory.State]

  private [ChallengeMM1] object ResFactory:
    final case class State(
        totalOpened: Int,
        computedResults: Chain[Int]
    ):
      def openOne: State =
        this.copy(totalOpened = this.totalOpened + 1)

      def closeOne(finalResult: Int): State =
        this.copy(computedResults = this.computedResults.append(finalResult))

    object State:
      val initial: State = State(
        totalOpened = 0,
        computedResults = Chain.empty
      )
    end State

    val make: IO[ResFactory] =
      IO.ref(State.initial).map { state =>
        new ResFactory:
          override val createRes: Resource[IO, Res] =
            Resource.make(
              acquire = state.update(_.openOne) >> IO.ref(0)
            )(
              release = counter =>
                counter.get.flatMap(result => state.update(_.closeOne(result)))
            ).map { counter =>
              new Res:
                override def put(value: Int): IO[Unit] =
                  counter.update(_ + value)

                override val get: IO[Int] =
                  counter.get
            }

          override val currentState: IO[State] =
            state.get
      }
  end ResFactory

  trait Solution:
    def run(
      resFactory: ResFactory,
      data: Vector[(Slow, Iffy, Magic)]
    ): IO[Option[(Int, Int)]]
  end Solution

  def runTests(solution: Solution): IO[Unit] =
    testInputs.traverse_ { testData =>
      runTest(solution, testData).flatMap {
        case Right(duration) =>
          IO.println(s"Challenge succeeded in ${duration.toMillis} milliseconds")

        case Left(msg) =>
          IO.println(s"Challenge failed: ${msg}")
      }
    }
  end runTests

  private def runTest(solution: Solution, testData: Vector[TestInput]): IO[Either[String, FiniteDuration]] =
    for
      resFactory <- ResFactory.make
      solutionOutput <- solution.run(resFactory, data = testData.map(_.toSolutionData)).timed
      (duration, result) = solutionOutput
      _ <- IO.println(s"Challenge result: ${result}")
      expectedResult = getExpectedOutput(testData)
      finalState <- resFactory.currentState
    yield
      (result, expectedResult) match
        case (Some((globalCounter1, globalCounter2)), Some((expectedComputedResults, expectedGlobalCounter1, expectedGlobalCounter2))) =>
          if (globalCounter1 != expectedGlobalCounter1) then
            Left("Wrong result for the first global counter")
          else if (globalCounter2 != expectedGlobalCounter2) then
            Left("Wrong result for the second global counter")
          else if (expectedComputedResults.exists(expectedComputedResult => !finalState.computedResults.contains(expectedComputedResult))) then
            Left("An expected computed result was not written to a Res")
          else if (finalState.totalOpened != (testData.size + 2)) then
            Left("Not enough resources where created")
          else if (finalState.totalOpened != finalState.computedResults.size) then
            Left("Not all opened resources where closed")
          else
            Right(duration)

        case (None, None) =>
          if (finalState.totalOpened != finalState.computedResults.size) then
            Left("Not all opened resources where closed")
          else
            Right(duration)

        case (Some(_), None) =>
          Left("Solution succeeded but it should have failed")

        case (None, Some(_)) =>
          Left("Solution failed but it should have succeeded")
  end runTest

  private def getExpectedOutput(testData: Vector[TestInput]): Option[(Chain[Int], Int, Int)] =
    val shouldFail = testData.exists { testInput =>
      (testInput.magicSecret > 0) &&
      (testData(testInput.magicSecret).slowValue > testInput.magicSecret) &&
      testData(testInput.magicSecret).isIffyBroken
    }

    Option.when(!shouldFail) {
      testData.foldMap { testInput =>
        (
          Chain.one(testInput.magicSecret + testInput.magicOp(testInput.slowValue)),
          math.max(testInput.magicSecret, 0),
          if testInput.magicSecret > 0 then
            val referencedTestInput = testData(testInput.magicSecret)
            if (referencedTestInput.slowValue > testInput.magicSecret)
              referencedTestInput.slowValue + referencedTestInput.iffyValue
            else
              referencedTestInput.slowValue
          else
            0
        )
      }
    }
  end getExpectedOutput

  private final case class TestInput(
    slowValue: Int,
    iffyValue: Int,
    isIffyBroken: Boolean,
    magicSecret: Int,
    magicOp: Int => Int
  ):
    def toSolutionData: (Slow, Iffy, Magic) =
      (
        Slow.make(value = slowValue),
        Iffy.make(value = iffyValue, isBroken = isIffyBroken),
        Magic.make(secret = magicSecret, op = magicOp)
      )
  end TestInput

  private val testInputs: List[Vector[TestInput]] = List(
    Vector(
      TestInput(slowValue = 10, iffyValue = 10, isIffyBroken = false, magicSecret = 0, magicOp = _ + 1)
    ),
    Vector(
      TestInput(slowValue = 10, iffyValue = 10, isIffyBroken = true, magicSecret = 0, magicOp = _ * 2)
    ),
    Vector(
      TestInput(slowValue = 50, iffyValue = 5, isIffyBroken = false, magicSecret = 3, magicOp = _ + 4),
      TestInput(slowValue = 100, iffyValue = 5, isIffyBroken = true, magicSecret = 0, magicOp = _ + 5),
      TestInput(slowValue = 20, iffyValue = 5, isIffyBroken = false, magicSecret = 3, magicOp = _ * 2),
      TestInput(slowValue = 40, iffyValue = 40, isIffyBroken = true, magicSecret = 0, magicOp =_ * 3)
    ),
    Vector(
      TestInput(slowValue = 1, iffyValue = 50, isIffyBroken = false, magicSecret = 3, magicOp = _ + 4),
      TestInput(slowValue = 100, iffyValue = 5, isIffyBroken = true, magicSecret = 0, magicOp = _ + 5),
      TestInput(slowValue = 20, iffyValue = 5, isIffyBroken = false, magicSecret = 3, magicOp = _ * 2),
      TestInput(slowValue = 2, iffyValue = 400, isIffyBroken = true, magicSecret = 0, magicOp = _ * 3)
    )
  )
end ChallengeMM1

object CE3MM1Solution extends ChallengeMM1.Solution:
  override def run(
    resFactory: ChallengeMM1.ResFactory,
    data: Vector[(ChallengeMM1.Slow, ChallengeMM1.Iffy, ChallengeMM1.Magic)]
  ): IO[Option[(Int, Int)]] =
    val (slows, iffies, magics) = data.unzip3
    (resFactory.createRes, resFactory.createRes, Supervisor[IO](await = false)).tupled.use {
      case (globalCounter1, globalCounter2, supervisor) =>
        // Start computing all slow values in background.
        slows.traverse { slow =>
          IO.deferred[Int].flatMap { df =>
            supervisor.supervise(slow.get.flatMap(df.complete)).as(df.get)
          }
        }.flatMap { slowValues =>
          logic(
            resFactory,
            slowValues,
            iffies,
            magics,
            globalCounter1,
            globalCounter2
          )
        } >>
        (globalCounter1.get, globalCounter2.get).tupled // Get the final values of all counters.
    }.option // Recover any failure with a None.
  end run

  private def logic(
    resFactory: ChallengeMM1.ResFactory,
    slowValues: Vector[IO[Int]],
    iffies: Vector[ChallengeMM1.Iffy],
    magics: Vector[ChallengeMM1.Magic],
    globalCounter1: ChallengeMM1.Res,
    globalCounter2: ChallengeMM1.Res
  ): IO[Unit] =
    // The logic is run in parallel for each magic and slow pair.
    (magics zip slowValues).parTraverse_ { case (magic, getMagicSlowValue) =>
      // Create a Res for each magic element.
      resFactory.createRes.use { localCounter =>
        magic.invoke { secretValue =>
          // Add the secret value to the local counter.
          localCounter.put(secretValue) >>
          IO.whenA(secretValue > 0) {
            // The program ensures the following index access are safe.
            val slowValueBySecretIndex = slowValues(secretValue)
            val iffyBySecretIndex = iffies(secretValue)

            // Add each non-negative secret value to the first global counter.
            globalCounter1.put(secretValue) >>
            // Get the slow and iffy values referenced by the secret as index.
            slowValueBySecretIndex.flatMap { slowValue =>
              // Add that slow value to the second global counter.
              globalCounter2.put(slowValue) >>
              // And, only if that slow value is grater than the secret value,
              // add the iffy value to the second global index as well.
              IO.whenA(slowValue > secretValue) {
                iffyBySecretIndex.get.flatMap { iffyValue =>
                  globalCounter2.put(iffyValue)
                }
              }
            }
          } >>
          // Pass the paired slow value to the magic operation.
          getMagicSlowValue
        }.flatMap { magicInvokeResultOnSlowValue =>
          // Add the result of magic.invoke with its paired slow value to the local counter.
          localCounter.put(magicInvokeResultOnSlowValue)
        }
      }
    }
  end logic
end CE3MM1Solution

object Main extends IOApp.Simple:
  override val run: IO[Unit] =
    ChallengeMM1.runTests(solution = CE3MM1Solution)
end Main
