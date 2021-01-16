package machines
package examples

import Machine._

val s = React((), (_: Any, i: Int) => Stop("Done"), Stop("Dry"))
val m = Emit("Hi", Stop(1))
val c = Stop(12)
val e = Error(new RuntimeException("oh"))

val ti = Distribute[Int]
val tt = Distribute[String]
val n1 = Process.Stage(m, discard, tt, ti)
val n2 = Process.Stage(m, unused, tt, discard)
