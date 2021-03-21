package machines

final class Tag[A](val description: String = "")

type Producer[+B, -C <: Pure]   = Process[Nothing, B, C, Any]
type Consumer[-A, -C <: Pure]   = Process[A, Nothing, C, Any]
type Transducer[-A, +B]         = Process[A, B, Pure, Any]
type Reducer[-A, +D]            = Process[A, Nothing, Pure, D]

enum Process[-A, +B, -C <: Pure, +D]:
  case Run(state0: Machine[A, B, C, D])
  case Reduce[A, D](process: Reducer[A, D]) extends Transducer[A, D]
  case Monitor[A, B, C <: Pure, D](process: Process[A, B, C, D], supervisor: Consumer[D, C]) extends Process[A, B, C, Any]
  case Recover[A, B, C <: Pure, D](process: Process[A, B, C, D], supervisor: Consumer[Throwable, C]) extends Process[A, B, C, D]
  case Repeat[A, B](process: Transducer[A, B]) extends Transducer[A, B]
  case Concat[A, B](processes: Transducer[A, B]*) extends Transducer[A, B]
  case Pipe[A, B, C <: Pure, X, D](stage1: Process[A, X, C, Any], stage2: Process[X, B, C, D]) extends Process[A, B, C, D]
  case Broadcast[A, C <: Pure](backlog: Int, receivers: Consumer[A, C]*) extends Consumer[A, C]
  case Balance[A, C <: Pure](receivers: Consumer[A, C]*) extends Consumer[A, C]
  case Concentrate[A, C <: Pure](senders: Producer[A, C]*) extends Producer[A, C]
  case Ref[A, A1 >: A](tag: Tag[A1]) extends Consumer[A, Pure]
  case Deref[B, B1 <: B](tag: Tag[B1]) extends Producer[B, Pure]
  case System[C <: Pure](processes: Process[Nothing, Nothing, C, Any]*) extends Process[Nothing, Nothing, C, Any]

  def :->[X >: B, Y](that: Transducer[X, Y]) = Pipe(this, that)
