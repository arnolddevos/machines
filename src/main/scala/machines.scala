package machines

enum Machine[-A, +B, +C]:
  case React[A, B, C, S]( state: S, accept: (S, A) => Machine[A, B, C], close: Machine[Nothing, B, C]) extends Machine[A, B, C]
  case Defer[A, B, C, S]( state: S, next: S => Machine[A, B, C]) extends Machine[A, B, C]
  case Emit( b: B, next: Machine[A, B, C])
  case Branch( react: Machine[A, B, C], emit: Machine[A, B, C])
  case Stop(c: C)
  case Error(e: Throwable)

type Producer[+A]       = Machine[Nothing, A, Unit]
type Reducer[-A, +B]    = Machine[A, Nothing, B]
type Transducer[-A, +B] = Machine[A, B, Unit]

import Machine._

def mealy[A, B, S](s0: S)(f: (S, A) => S, g: (S, A) => B, p: (S, A) => Boolean): Transducer[A, B] =
  class Plan:
    val phase2f = phase2
    val stop = Stop(()) 

    def phase1(s1: S): Transducer[A, B] =
      React(s1, phase2f, stop)

    def phase2(s1: S, a: A): Transducer[A, B] =
      if p(s1, a) then Emit(g(s1, a), phase1(f(s1, a)))
      else phase1(f(s1, a))
      
  Plan().phase1(s0)

def moore[A, B, S](s0: S)(f: (S, A) => S, g: S => B, p: S => Boolean): Transducer[A, B] =
  class Plan:
    val phase1f = (s: S, a: A) => phase1(f(s, a))
    val stop = Stop(()) 

    def phase1(s1: S): Transducer[A, B] =
      if p(s1) then Emit(g(s1), phase2(s1))
      else phase2(s1)

    def phase2(s1: S): Transducer[A, B] =
      React(s1, phase1f, stop)
      
  Plan().phase1(s0)

def filterMap[A, B](f: A => B, p: A => Boolean): Transducer[A, B] = 
  mealy(())((s, _) => s, (_, a) => f(a), (_, a) => p(a))
