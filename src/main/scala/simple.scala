package machines
import Machine._

type SimpleMachine[-A, +B] = Machine[A, B, Pure, Any]

def mealy[A, B, S](s0: S)(f: (S, A) => S, g: (S, A) => B, p: (S, A) => Boolean): SimpleMachine[A, B] =
  class Plan:
    val phase2f = phase2
    val stop = Return(()) 

    def phase1(s1: S): SimpleMachine[A, B] =
      React(s1, phase2f, stop)

    def phase2(s1: S, a: A): SimpleMachine[A, B] =
      if p(s1, a) then Emit(g(s1, a), phase1(f(s1, a)))
      else phase1(f(s1, a))
      
  Plan().phase1(s0)

def moore[A, B, S](s0: S)(f: (S, A) => S, g: S => B, p: S => Boolean): SimpleMachine[A, B] =
  class Plan:
    val phase1f = (s: S, a: A) => phase1(f(s, a))
    val stop = Return(()) 

    def phase1(s1: S): SimpleMachine[A, B] =
      if p(s1) then Emit(g(s1), phase2(s1))
      else phase2(s1)

    def phase2(s1: S): SimpleMachine[A, B] =
      React(s1, phase1f, stop)
      
  Plan().phase1(s0)

def filterMap[A, B](f: A => B, p: A => Boolean): SimpleMachine[A, B] = 
  mealy(())((s, _) => s, (_, a) => f(a), (_, a) => p(a))

def filter[A](p: A => Boolean) = filterMap((a: A)=> a, p)

def collect[A, B](pf: PartialFunction[A, B]) = filterMap(pf, pf.isDefinedAt)

def queue[A](backlog: Int): SimpleMachine[A, A] =
  class Plan:
    case class Queue(n: Int, left: List[A], right: List[A], closed: Boolean):
      def machine: SimpleMachine[A, A] = 
        if closed then
          if n > 0 then dequeue(this) else Return(())
        else
          if n > 0 && n < backlog then
            Branch(React(this, enqueue, Step(this, close)), Step(this, dequeue))
          else if n > 0 then dequeue(this)
          else React(this, enqueue, Step(this, close))

    val enqueue: (Queue, A) => SimpleMachine[A, A] =
      (q, a) => Queue(q.n+1, a :: q.left, q.right, q.closed).machine
    
    val dequeue: Queue => SimpleMachine[A, A] =
      q =>
        def right = if q.right.nonEmpty then q.right else q.left.reverse
        def left = if q.right.nonEmpty then q.left else Nil
        Emit(right.head, Queue(q.n-1, left, right.tail, q.closed).machine)

    val close: Queue => SimpleMachine[A, A] =
      q => q.copy(closed=true).machine

  Plan().Queue(0, Nil, Nil, false).machine

def buffer[A]: SimpleMachine[A, A] =

  class Plan:
    val empty: SimpleMachine[A, A] = React((), accept, stop)
    val accept = (_: Any, a: A) => Emit(a, empty)
    val stop = Return(())

  Plan().empty
