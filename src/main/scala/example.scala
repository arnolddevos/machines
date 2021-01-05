package machines
package examples

import Machine._

val s = React((), (_: Any, i: Int) => Stop("Done"), (_: Any) => Stop("Dry"))
val m = Emit("Hi", (), _ => Stop(1))
val c = Stop(12)
val e = Error("oh")

val ti = Distribute[Int]
val tt = Distribute[String]
val n1 = Node.Process(m, discard, tt, ti, unused)
val n2 = Node.Process(m, unused, tt, discard, unused)
