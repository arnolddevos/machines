package machines

sealed trait Pure
object Pure extends Pure

trait Impure extends Pure
object Impure extends Impure

enum Machine[-A, +B, -C <: Pure, +D]:
  case React[A, B, C <: Pure, D, S]( state: S, accept: (S, A) => Machine[A, B, C, D], close: Machine[A, B, C, D]) extends Machine[A, B, C, D]
  case Step[A, B, C <: Pure, D, S]( state: S, advance: S => Machine[A, B, C, D]) extends Machine[A, B, C, D]
  case Effect[A, B, C <: Pure, D, S]( state: S, run: (S, C) => Machine[A, B, C, D]) extends Machine[A, B, C, D]
  case Emit( b: B, continue: Machine[A, B, C, D])
  case Branch( react: Machine[A, B, C, D], emit: Machine[A, B, C, D])
  case Return(d: D)
  case Error(e: Throwable)
  case Stop

  def mapEmit[X](f: B => X): Machine[A, X, C, D] =
    this match 
      case React(s, g, d) => React(s, (s, a) => g(s, a).mapEmit(f), d.mapEmit(f))
      case Step(s, g) => Step(s, s => g(s).mapEmit(f))
      case Emit(b, d) => Emit(f(b), d.mapEmit(f))
      case Effect(s, g) => Effect(s, (s, c) => g(s, c).mapEmit(f))
      case Branch(l, r) => Branch(l.mapEmit(f), r.mapEmit(f))
      case Return(d) => Return(d)
      case Error(t) => Error(t)
      case Stop => Stop

  def mapAccept[X](f: X => A): Machine[X, B, C, D] =
    this match 
      case React(s, g, d) => React(s, (s, d) => g(s, f(d)).mapAccept(f), d.mapAccept(f))
      case Step(s, g) => Step(s, s => g(s).mapAccept(f))
      case Effect(s, g) => Effect(s, (s, c) => g(s, c).mapAccept(f))
      case Emit(b, d) => Emit(b, d.mapAccept(f))
      case Branch(l, r) => Branch(l.mapAccept(f), r.mapAccept(f))
      case Return(d) => Return(d)
      case Error(t) => Error(t)
      case Stop => Stop

  def map[X](f: D => X): Machine[A, B, C, X] =
    this match 
      case React(s, g, d) => React(s, (s, a) => g(s, a).map(f), d.map(f))
      case Step(s, g) => Step(s, s => g(s).map(f))
      case Effect(s, g) => Effect(s, (s, c) => g(s, c).map(f))
      case Emit(b, d) => Emit(b, d.map(f))
      case Branch(l, r) => Branch(l.map(f), r.map(f))
      case Return(d) => Return(f(d))
      case Error(t) => Error(t)
      case Stop => Stop

  def flatMap[A1 <: A, B1 >: B, C1 <: C, X](f: D => Machine[A1, B1, C1, X]): Machine[A1, B1, C1, X] = 
    this match 
      case React(s, g, d) => React(s, (s, a) => g(s, a).flatMap(f), d.flatMap(f))
      case Step(s, g) => Step(s, s => g(s).flatMap(f))
      case Effect(s, g) => Effect(s, (s, c) => g(s, c).flatMap(f))
      case Emit(b, d) => Emit(b, d.flatMap(f))
      case Branch(l, r) => Branch(l.flatMap(f), r.flatMap(f))
      case Return(d) => f(d)
      case Error(t) => Error(t)
      case Stop => Stop

  def fold[X](f: B => X, h: D => X): Machine[A, X, C, Any] =
    this match
      case React(s, g, d) => React(s, (s, a) => g(s, a).fold(f, h), d.fold(f, h))
      case Step(s, g) => Step(s, s => g(s).fold(f, h))
      case Effect(s, g) => Effect(s, (s, c) => g(s, c).fold(f, h))
      case Emit(b, d) => Emit(f(b), d.fold(f, h))
      case Branch(l, r) => Branch(l.fold(f, h), r.fold(f, h))
      case Return(d) => Emit(h(d), Return(()))
      case Error(t) => Error(t)
      case Stop => Stop

  def recover[A1 <: A, B1 >: B, C1 <: C, D1 >: D](f: Throwable => Machine[A1, B1, C1, D1]): Machine[A1, B1, C1, D1] =
    this match 
      case React(s, g, d) => React(s, (s, a) => g(s, a).recover(f), d.recover(f))
      case Step(s, g) => Step(s, s => g(s).recover(f))
      case Effect(s, g) => Effect(s, (s, c) => g(s, c).recover(f))
      case Emit(b, d) => Emit(b, d.recover(f))
      case Branch(l, r) => Branch(l.recover(f), r.recover(f))
      case Return(d) => Return(d)
      case Error(t) => f(t)
      case Stop => Stop

  @annotation.tailrec
  final def seekBranch: Machine[A, B, C, D] =
    this match 
      case Step(s, f) => f(s).seekBranch
      case m => m

  final def seekReact: Machine[A, B, C, D] =
    this match 
      case Step(s, f) => f(s).seekReact
      case Branch(r, e) => 
        r.seekReact match
          case m @ React(_, _, _) => m
          case m1 => 
            e.seekReact match
              case m @ React(_, _, _) => m
              case _ => m1
      case m => m

  final def seekEmit: Machine[A, B, C, D] =
    this match 
      case Step(s, f) => f(s).seekEmit
      case Branch(r, e) => 
        e.seekEmit match
          case m @ Emit(_, _) => m
          case m1 => 
            r.seekEmit match 
              case m @ Emit(_, _) => m
              case _ => m1
      case m => m

  final def seekEmitStop: Machine[A, B, C, D] =
    this match 
      case Step(s, f) => f(s).seekEmitStop
      case Branch(r, e) => 
        e.seekEmitStop match
          case m @ (Emit(_, _) | Return(_)) => m
          case m1=> 
            r.seekEmitStop match
              case m @ (Emit(_, _) | Return(_)) => m
              case _ => m1
      case m => m
