//> using scala 3.4.2
//> using dep com.github.ichoran::kse3-flow:0.3.2
//> using mainClass dvsm.mm1.Kse3MM1

// Run with scala-cli --jvm 21 KseMM1.scala ChallengeMM1.scala

package dvsm.mm1

object Kse3MM1:
  import kse.basics.*
  import kse.flow.*
  import ChallengeMM1.*

  def candidate(rsOpen: () => Res, in: Seq[(Slow, Iffy, Magic)]): Option[(Int, Int)] =
    // If anything goes wrong, we bail on the whole computation
    Fu.group{
      // Global resource used to calculate second sum
      Resource(rsOpen())(_.close()): r0 =>

        // We'll need every slow value, so run them all
        val loaded = in.toArray.map(sim => sim.copy(_1 = Fu(sim._1.get)))

        // Now step through everything--may as well give everything its own future
        val eval = loaded.map{ case (sv, iffy, magic) =>
          Fu:
            var secret = 0

            // Entry-specific resource
            Resource(rsOpen())(_.close()): ri =>
              ri.put(magic.invoke{ i => secret = i; sv.? })
              ri.put(secret)

            // Secret-as-index calculation
            if secret >= 0 then
              val (svi, iffi, _) = loaded(secret)
              r0.put(svi.? + (if svi.? > secret then iffi.get else 0))

            secret max 0
        }
        (eval.map(_.?).sum, r0.get)
    }.ask().toOption

  def main(args: Array[String]): Unit =
    ChallengeMM1.printTests(candidate)
