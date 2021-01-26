package machines

import Machine._

trait Actor[A, B]:

  opaque type Cont = Machine[A, B, Any]

  final def process: Process[A, B, Any] = 
    Process.Run(machine)
  final def machine: Machine[A, B, Any] =
    Defer(this, a => a.start)
  final def react(cont: A => Cont): Cont = 
    React(cont, (c, a) => c(a), stop)
  final def send(b: B)( cont: => Cont): Cont = 
    Emit(b, Defer(() => cont, c => c()))
  final def stop: Cont =
    Stop(())
  def start: Cont
