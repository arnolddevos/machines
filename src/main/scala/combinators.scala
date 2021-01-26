package machines

final class Tag[A]

type Producer[+B] = Process[Nothing, B, Any]
type Consumer[-A] = Process[A, Nothing, Any]
type Reducer[-A, +C] = Process[A, Nothing, C] 
type Transducer[-A, +B] = Process[A, B, Any]

enum Process[-A, +B, +C]:
  case Transducer(state0: Machine[A, B, C])
  case Pipe[A, B, C](sender: Process[A, B, C], receiver: Consumer[B]) extends Process[A, Nothing, C]
  case Monitor[A, B, C](subject: Process[A, B, C], supervisor: Consumer[C]) extends Process[A, B, Any]
  case Broadcast[A](backlog: Int, receivers: Consumer[A]*) extends Consumer[A]
  case Distribution[A](receivers: Consumer[A]*) extends Consumer[A]
  case Ref[A, A1 >: A](tag: Tag[A1]) extends Consumer[A]
  case Topic[B, B1 <: B](tag: Tag[B1], name: String) extends Producer[B]
  case Output[A, A1 >: A](tag: Tag[A1]) extends Consumer[A]
  case Input[B, B1 <: B](tag: Tag[B1]) extends Producer[B]
  case System(processes: Process[Nothing, Nothing, Any]*) extends Process[Nothing, Nothing, Any]

import Process._
