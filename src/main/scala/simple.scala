package machines

type SimpleMachine[-A, +B] = Machine[A, B, Any]

import Machine._

def mealy[A, B, S](s0: S)(f: (S, A) => S, g: (S, A) => B, p: (S, A) => Boolean): SimpleMachine[A, B] =
  class Plan:
    val phase2f = phase2
    val stop = Stop(()) 

    def phase1(s1: S): SimpleMachine[A, B] =
      React(s1, phase2f, stop)

    def phase2(s1: S, a: A): SimpleMachine[A, B] =
      if p(s1, a) then Emit(g(s1, a), phase1(f(s1, a)))
      else phase1(f(s1, a))
      
  Plan().phase1(s0)

def moore[A, B, S](s0: S)(f: (S, A) => S, g: S => B, p: S => Boolean): SimpleMachine[A, B] =
  class Plan:
    val phase1f = (s: S, a: A) => phase1(f(s, a))
    val stop = Stop(()) 

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


