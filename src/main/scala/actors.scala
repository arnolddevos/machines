package machines

import Machine._

val apply0 = [A] => (f: () => A) => f()
val apply1 = [A, B] => (f: A => B, a: A) => f(a)

abstract class Actor[A, B, C]:

  opaque type Cont = Machine[A, B, C]

  final def process: Process[A, B, C] = 
    Process.Run(machine)
  final def machine: Machine[A, B, C] =
    Defer(this, a => a.start)
  final def reactOr(alt: => Cont)(cont: A => Cont): Cont = 
    React(cont, apply1[A, Cont], Defer(() => alt, apply0[Cont]))
  final def send(b: B)( cont: => Cont): Cont = 
    Emit(b, Defer(() => cont, apply0[Cont]))
  final def stop(c: C): Cont =
    Stop(c)
  final def error(t: Throwable): Cont =
    Error(t) 
  final def branch(l: => Cont)(r: => Cont): Cont =
    Branch(Defer(() => l, apply0[Cont]), Defer(() => r, apply0[Cont]))
  final def react(cont: A => Cont)(using result: Unit <:< C): Cont = 
    React(cont, apply1[A, Cont], stop(result(())))  
  
  def start: Cont

