package machines
package examples

import Machine._

val s = React((), (_: Any, i: Int) => Stop("Done"), Stop("Dry"))
val m = Emit("Hi", Stop(1))
val c = Stop(12)
val e = Error(new RuntimeException("oh"))


object Doubler extends Actor[Int, Int]:
  def start = react { i => send(i*2)(start) }

object Incrementer extends Actor[Int, Int]:
  def start = react { i => send(i+1)(start) }

