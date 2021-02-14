package machines

enum BroadcastState[A, B, C]:
  case Quiescent(cells: List[Cell[A, B, C]])
  case Sending(a: A, pending: List[Cell[A, B, C]], succeeded: List[Cell[A, B, C]])
  case Closing(pending: List[Cell[A, B, C]])

import BroadcastState._
import Machine._
import Reaction._

abstract class BroadcastSynapse extends Synapse:
  type A1  
  type B1 <: A2
  type C1
  type A2
  type B2
  type C2
  val left: Cell[A1, B1, C1]
  var right: BroadcastState[A2, B2, C2]
  var live = false

  @annotation.tailrec
  final def broadcast(state: BroadcastState[A2, B2, C2], failed: List[Cell[A2, B2, C2]]=Nil): BroadcastState[A2, B2, C2] =
    state match
      case Sending(a, c :: pending, succeeded) =>
        c.accept(a) match 
          case Accepted => broadcast(Sending(a, pending, c :: succeeded), failed)
          case Rejected => broadcast(Sending(a, pending, succeeded), c :: failed)
          case Blocked  => broadcast(Sending(a, pending, succeeded), failed)
      case Closing(c :: pending) =>
        c.close match 
          case Accepted | Blocked => broadcast(Closing(pending), failed)
          case Rejected => broadcast(Closing(pending), c :: failed)
      case Sending(a, Nil, succeeded) =>
          if failed.isEmpty then Quiescent(succeeded)
          else Sending(a, failed, succeeded)
      case Closing(Nil) =>
          if failed.isEmpty then Quiescent(Nil)
          else Closing(failed)    
      case Quiescent(_) => state 

  def run: Boolean =
    val state = right match
      case s @ Quiescent(cells) =>
        left.state.seekEmit match 
          case l @ Emit(b, c) =>
            if cells.isEmpty then 
              left.block(l)
              live = false
              s
            else
              left.continue(c.seekBranch)
              broadcast(Sending(b, cells, Nil))
          case l @ Stop(_) =>
            if cells.isEmpty then
              left.continue(l)
              live = false
              s
            else
              left.continue(l)
              broadcast(Closing(cells))
          case _ => s
      case s =>
        broadcast(s)

    val changed = right != state
    right = state
    changed

object BroadcastSynapse:
  def apply[Al, Bl <: Ar, Cl, Ar, Br, Cr](l: Cell[Al, Bl, Cl], r: Iterable[Cell[Ar, Br, Cr]]) = 
    new BroadcastSynapse:
      type A1 = Al
      type B1 = Bl
      type C1 = Cl
      type A2 = Ar
      type B2 = Br
      type C2 = Cr
      val left = l
      var right = Quiescent(r.toList)
      for c <- r do c.open
      left.subscribe