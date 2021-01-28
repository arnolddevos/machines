package machines

final class Tag[A](val description: String = "")

type Producer[+B]       = Process[Nothing, B, Any]
type Consumer[-A]       = Process[A, Nothing, Any]
type Transducer[-A, +B] = Process[A, B, Any]
type Reducer[-A, +C]    = Process[A, Nothing, C]

enum Process[-A, +B, +C]:
  case Run(state0: Machine[A, B, C])
  case Reduce[A, C](process: Reducer[A, C]) extends Transducer[A, C]
  case Monitor[A, B, C](process: Process[A, B, C], supervisor: Consumer[C]) extends Transducer[A, B]
  case Repeat[A, B](process: Transducer[A, B]) extends Transducer[A, B]
  case Concat[A, B](processes: Transducer[A, B]*) extends Transducer[A, B]
  case Pipe[A, B, X](stage1: Transducer[A, X], stage2: Transducer[X, B]) extends Transducer[A, B]
  case Broadcast[A](backlog: Int, receivers: Consumer[A]*) extends Consumer[A]
  case Balance[A](receivers: Consumer[A]*) extends Consumer[A]
  case Concentrate[A](senders: Producer[A]*) extends Producer[A]
  case Ref[A, A1 >: A](tag: Tag[A1]) extends Consumer[A]
  case Deref[B, B1 <: B](tag: Tag[B1]) extends Producer[B]
  case Output[A, A1 >: A](tag: Tag[A1]) extends Consumer[A]
  case Input[B, B1 <: B](tag: Tag[B1]) extends Producer[B]
  case System(processes: Process[Nothing, Nothing, Any]*) extends Process[Nothing, Nothing, Any]

  def :->[X >: B, Y](that: Transducer[X, Y]) = Pipe(this, that)

import Process._
