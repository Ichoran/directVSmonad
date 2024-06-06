package dvsm.mm1

import scala.util.*

class ChallengeMM1(values: Seq[(Int, Int, Boolean, Int, Int => Int)]):
  import ChallengeMM1._

  class MySlow(private[ChallengeMM1] val value: Int) extends Slow:
    def get: Int =
      val t0 = System.nanoTime
      while System.nanoTime - t0 < 10000000*value do
        Thread.sleep(1)
      value

  class MyIffy(private[ChallengeMM1] val value: Int, private[ChallengeMM1] val broken: Boolean) extends Iffy:
    def get: Int =
      if broken then throw new Exception("broken")
      else value

  class MyMagic(private[ChallengeMM1] val secret: Int, op: Int => Int) extends Magic:
    def invoke(f: Int => Int) = op(f(secret))

  class MyRes() extends Res:
    MyRes.counter.getAndIncrement
    private var closed: Boolean = false
    private var sum: Int = 0
    def put(value: Int) = synchronized:
      if closed then throw new Exception("closed")
      MyRes.accumulator.getAndAdd(value)
      sum += value
    def get: Int = synchronized:
      sum
    def close(): Unit = synchronized:
      if !closed then MyRes.counter.getAndAdd(1000)
      closed = true
  object MyRes:
    val counter = new java.util.concurrent.atomic.AtomicInteger(0)
    val accumulator = new java.util.concurrent.atomic.AtomicInteger(0)
    def open(): Res = new MyRes()

  private val input = values.map{ case (s, i, b, m, o) => (MySlow(s), MyIffy(i, b), MyMagic(m, o)) }

  def run(candidate: (() => Res, Seq[(Slow, Iffy, Magic)]) => Option[(Int, Int)]): Either[String, Double] =
    val t0 = System.nanoTime
    val answer =
      try candidate(() => MyRes.open(), input)
      catch case e if control.NonFatal(e) => return Left("Terminated with exception")
    val t1 = System.nanoTime
    val dt = (t1 - t0)/1e9
    MyRes.counter.get match
      case x if x%1000 != x/1000 => return Left(s"${x%1000} resources opened but ${x/1000} closed")
      case _ =>
    val failings = input.zipWithIndex.collect{
      case ((_, _, m), i) if m.secret >= 0 && input(m.secret)._1.value >= m.secret && input(m.secret)._2.broken => (m.secret, input(m.secret)._1.value)
    }
    if failings.nonEmpty then
      answer match
        case Some(_) => return Left(s"Successful completion reported but should have failed at indices ${failings.map(_._1).mkString(", ")}")
        case _ =>
      val longest = 0.05 + 1.5e-2*failings.map(_._2).min
      if dt > longest then Left(f"Correct (failure) answer but too slow: needed less than $longest%.3f s but took $dt%.3f s")
      else Right(dt)
    else
      if MyRes.counter.get % 1000 != 1 + input.length then return Left(s"Wrong number of resources: ${MyRes.counter.get % 1000}")
      val magicSum = input.map(_._3.secret max 0).sum
      val jumpSum = input.collect{ case (_, _, m) if m.secret >= 0 => m.secret }.map{ i =>
        val sv = input(i)._1.value
        if sv > i then sv + input(i)._2.value else sv
      }.sum
      val resAcc =
        input.map{ case (s, _, m) => m.invoke(i => s.value) + m.secret }.sum + jumpSum
      answer match
        case None => return Left(s"Answered failure, but operation should have succeeded")
        case Some((i1, i2)) =>
          if i1 != magicSum then return Left(s"First sum wrong: $i1")
          if i2 != jumpSum then return Left(s"Second sum wrong: $i2")
          if MyRes.accumulator.get != resAcc then return Left(s"Wrong value written into resources: ${MyRes.accumulator.get}")
      val longest = 0.05 + 1.5e-2*input.map(_._1.value).max
      if dt > longest then Left(f"Correct (success) answer but too slow: needed less than $longest%.3f s but took $dt%.3f s")
      else Right(dt)

object ChallengeMM1:
  trait Slow:
    def get: Int

  trait Iffy:
    def get: Int

  trait Magic:
    def invoke(f: Int => Int): Int

  trait Res extends java.io.Closeable:
    def get: Int
    def put(value: Int): Unit

  val testValues: Vector[Seq[(Int, Int, Boolean, Int, Int => Int)]] = Vector(
    Seq(
      (10, 10, false, 0, _ + 1)
    ),
    Seq(
      (10, 10, true, 0, _ * 2)
    ),
    Seq(
      (50, 5, false, 3, _ + 4),
      (100, 5, true, 0, _ + 5),
      (20, 5, false, 3, _ * 2),
      (40, 40, true, 0, _ * 3)
    ),
    Seq(
      (1, 50, false, 3, _ + 4),
      (100, 5, true, 0, _ + 5),
      (20, 5, false, 3, _ * 2),
      (2, 400, true, 0, _ * 3)
    )
  )

  def runTests(candidate: (() => Res, Seq[(Slow, Iffy, Magic)]) => Option[(Int, Int)]): Vector[Either[String, Double]] =
    testValues.map{ s =>
      val mm1 = ChallengeMM1(s)
      mm1.run(candidate)
    }

  def printTests(candidate: (() => Res, Seq[(Slow, Iffy, Magic)]) => Option[(Int, Int)]): Unit =
    runTests(candidate).foreach{
      case Left(e) => println(s"Challenge failed.\n  $e")
      case Right(t) => println(f"Challenge succeeded in $t%.3f s")
    }
