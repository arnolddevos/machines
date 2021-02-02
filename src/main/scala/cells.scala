package machines

import Machine._

class Cell[A, B, C](private var track: Machine[A, B, C], private var fanIn: Int = 0):
  def state = track
  def accept[S](f: (S, A) => Machine[A, B, C], s: S, a: A): Unit =
    track = f(s, a).seekBranch
  def open: Unit =
    fanIn += 1
  def close(m: Machine[A, B, C]): Unit =
    fanIn -= 1
    if fanIn == 0 then track = m.seekBranch
  def stop(c: C): Unit =
    track = Stop(c)
  def continue(m: Machine[A, B, C]): Unit =
    track = m.seekBranch

trait Synapse:
  def live: Boolean
  final def fire: Boolean = if live then run else false
  protected def run: Boolean

trait CellPair:
  type A1
  type B1
  type C1
  type A2
  type B2
  type C2
  val left: Cell[A1, B1, C1]
  val right: Cell[A2, B2, C2]
   
abstract class CommonSynapse extends Synapse with CellPair:
  type B1 <: A2
  var live: Boolean = true

  def run: Boolean =

    val r1 = right.state.seekReact
    
    r1 match
      case React(s, f, r2) =>
        val l1 = left.state.seekEmit
        l1 match
          case Emit(a, l2) =>
            right.accept(f, s, a)
            left.continue(l2)
            true
          case Stop(x) =>
            right.close(r2)
            left.stop(x)
            live = false
            true
          case Error(t) => throw t
          case _ => false
      case Stop(x) => 
        right.stop(x)
        live = false
        true
      case Error(t) => throw t
      case _ => false
      

object CommonSynapse:
  def apply[Al, Bl <: Ar, Cl, Ar, Br, Cr](l: Cell[Al, Bl, Cl], r: Cell[Ar, Br, Cr]) = 
    new CommonSynapse:
      type A1 = Al
      type B1 = Bl
      type C1 = Cl
      type A2 = Ar
      type B2 = Br
      type C2 = Cr
      val left = l
      val right = r
      right.open


abstract class OneShotSynapse extends Synapse with CellPair:
  type C1 <: A2
  var live: Boolean = true
  var closePending = false

  def run: Boolean =

    val r1 = right.state.seekReact
    
    r1 match
      case React(s, f, r2) =>
        if closePending then
          right.close(r2)
          live = false
          true
        else
          val l1 = left.state.seekEmitStop
          l1 match
            case Stop(a) =>
              right.accept(f, s, a)
              left.stop(a)
              live = true
              closePending = true
              true
            case Error(t) => throw t
            case _ => false
      case Stop(x) => 
        right.stop(x)
        live = false
        true
      case Error(t) => throw t
      case _ => false

object OneShotSynapse:
  def apply[Al, Bl, Cl <: Ar, Ar, Br, Cr](l: Cell[Al, Bl, Cl], r: Cell[Ar, Br, Cr]) = 
    new OneShotSynapse:
      type A1 = Al
      type B1 = Bl
      type C1 = Cl
      type A2 = Ar
      type B2 = Br
      type C2 = Cr
      val left = l
      val right = r
      right.open

