package machines
package examples

import Machine._

val s = React((), (_: Any, i: Int) => Return("Done"), Return("Dry"))
val m = Emit("Hi", Return(1))
val d = Return(12)
val e = Error(new RuntimeException("oh"))


object Doubler extends Actor[Int, Int, Any]:
  def start = react { i => send(i*2)(start) }

object Incrementer extends Actor[Int, Int, Any]:
  def start = react { i => send(i+1)(start) }
