package machines

class Cell[A, B, C](var state: Machine[A, B, C], var fanIn: Int = 0)

import Machine._

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
            right.state = f(s, a).seekBranch
            left.state  = l2.seekBranch
            true
          case Stop(_) =>
            right.fanIn -= 1
            if right.fanIn == 0 then right.state = r2.seekBranch
            left.state = l1
            live = false
            true
          case Error(t) => throw t
          case _ => false
      case Stop(_) => 
        right.fanIn -= 1
        right.state = r1
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
      right.fanIn += 1


abstract class OneShotSynapse extends Synapse with CellPair:
  type C1 <: A2
  var live: Boolean = true
  var closePending = false

  def run: Boolean =

    val r1 = right.state.seekReact
    
    r1 match
      case React(s, f, r2) =>
        if closePending then
          right.fanIn -= 1
          if right.fanIn == 0 then right.state = r2.seekBranch
          live = false
          true
        else
          val l1 = left.state.seekEmitStop
          l1 match
            case Stop(a) =>
              right.state = f(s, a).seekBranch
              left.state = l1
              live = true
              closePending = true
              true
            case Error(t) => throw t
            case _ => false
      case Stop(_) => 
        right.fanIn -= 1
        right.state = r1
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
      right.fanIn += 1

