package machines

import Machine._

val apply0 = [A] => (f: () => A) => f()
val apply1 = [A, B] => (f: A => B, a: A) => f(a)

abstract class Actor[A, B, D]:

  opaque type Cont = Machine[A, B, Pure, D]

  final def process: Process[A, B, Pure, D] = 
    Process.Run(machine)

  final def machine: Machine[A, B, Pure, D] =
    Step(this, a => a.start)

  final def reactOr(alt: => Cont)(cont: A => Cont): Cont = 
    React(cont, apply1[A, Cont], Step(() => alt, apply0[Cont]))

  final def send(b: B)( cont: => Cont): Cont = 
    Emit(b, Step(() => cont, apply0[Cont]))

  final def stop(d: D): Cont =
    Return(d)

  final def error(t: Throwable): Cont =
    Error(t) 

  final def branch(l: => Cont)(r: => Cont): Cont =
    Branch(Step(() => l, apply0[Cont]), Step(() => r, apply0[Cont]))
    
  final def react(cont: A => Cont)(using result: Unit <:< D): Cont = 
    React(cont, apply1[A, Cont], stop(result(())))  
  
  def start: Cont

