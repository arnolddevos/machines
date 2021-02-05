package machines

enum Machine[-A, +B, +C]:
  case React[A, B, C, S]( state: S, accept: (S, A) => Machine[A, B, C], close: Machine[A, B, C]) extends Machine[A, B, C]
  case Defer[A, B, C, S]( state: S, advance: S => Machine[A, B, C]) extends Machine[A, B, C]
  case Emit( b: B, continue: Machine[A, B, C])
  case Branch( react: Machine[A, B, C], emit: Machine[A, B, C])
  case Stop(c: C)
  case Error(e: Throwable)

  @annotation.tailrec
  final def seekBranch: Machine[A, B, C] =
    this match 
      case Defer(s, f) => f(s).seekBranch
      case m => m

  final def seekReact: Machine[A, B, C] =
    this match 
      case Defer(s, f)  => f(s).seekReact
      case Branch(r, e) => 
        r.seekReact match
          case m @ React(_, _, _) => m
          case _ => e.seekReact
      case m => m

  final def seekEmit: Machine[A, B, C] =
    this match 
      case Defer(s, f)  => f(s).seekEmit
      case Branch(r, e) => 
        e.seekEmit match
          case m @ Emit(_, _) => m
          case _ => e.seekEmit
      case m => m

  final def seekEmitStop: Machine[A, B, C] =
    this match 
      case Defer(s, f)  => f(s).seekEmitStop
      case Branch(r, e) => 
        e.seekEmitStop match
          case m @ (Emit(_, _) | Stop(_)) => m
          case _ => e.seekEmitStop
      case m => m
