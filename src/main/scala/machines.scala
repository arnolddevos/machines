package machines

enum Machine[-A, +B, +C, +D]:
  case React[A, B, C, D, S](s: S, f: (S, A) => Machine[A, B, C, D], g: S => Machine[A, B, C, D]) extends Machine[A, B, C, D]
  case Emit[A, B, C, D, S](b: B, s: S, f: S => Machine[A, B, C, D]) extends Machine[A, B, C, D]
  case Stop(c: C)
  case Error(e: D)
  
import Machine._

type SimpleMachine[-A, +B] = Machine[A, B, Unit, Nothing]

def mealy[A, B, S](s0: S)(f: (S, A) => S, g: (S, A) => B, p: (S, A) => Boolean): SimpleMachine[A, B] =
  class Plan:
    val phase1f = phase1
    val phase2f = phase2
    val stop = (s: S) => Stop(()) 

    def phase1(s1: S): SimpleMachine[A, B] =
      React(s1, phase2f, stop)

    def phase2(s1: S, a: A): SimpleMachine[A, B] =
      if p(s1, a) then Emit(g(s1, a), f(s1, a), phase1f)
      else phase1(f(s1, a))
      
  Plan().phase1(s0)

def moore[A, B, S](s0: S)(f: (S, A) => S, g: S => B, p: S => Boolean): SimpleMachine[A, B] =
  class Plan:
    val phase1f = (s: S, a: A) => phase1(f(s, a))
    val phase2f = phase2
    val stop = (s: S) => Stop(()) 

    def phase1(s1: S): SimpleMachine[A, B] =
      if p(s1) then Emit(g(s1), s1, phase2f)
      else phase2(s1)

    def phase2(s1: S): SimpleMachine[A, B] =
      React(s1, phase1f, stop)
      
  Plan().phase1(s0)

def filterMap[A, B](f: A => B, p: A => Boolean): SimpleMachine[A, B] = 
  mealy(())((s, _) => s, (_, a) => f(a), (_, a) => p(a))
