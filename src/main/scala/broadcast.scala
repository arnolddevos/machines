package machines

enum BroadcastState[A, B, D]:
  case Quiescent(cells: List[Cell[A, B, D]])
  case Sending(a: A, pending: List[Cell[A, B, D]], succeeded: List[Cell[A, B, D]])
  case Closing(pending: List[Cell[A, B, D]])

import BroadcastState._
import Machine._
import Reaction._

abstract class BroadcastSynapse extends Synapse:
  type A1  
  type B1 <: A2
  type D1
  type A2
  type B2
  type D2
  val left: Cell[A1, B1, D1]
  var right: BroadcastState[A2, B2, D2]
  var live = false

  @annotation.tailrec
  final def broadcast(state: BroadcastState[A2, B2, D2], failed: List[Cell[A2, B2, D2]]=Nil): BroadcastState[A2, B2, D2] =
    state match
      case Sending(a, d :: pending, succeeded) =>
        d.accept(a) match 
          case Accepted => broadcast(Sending(a, pending, d :: succeeded), failed)
          case Rejected => broadcast(Sending(a, pending, succeeded), d :: failed)
          case Blocked  => broadcast(Sending(a, pending, succeeded), failed)
      case Closing(d :: pending) =>
        d.close match 
          case Accepted | Blocked => broadcast(Closing(pending), failed)
          case Rejected => broadcast(Closing(pending), d :: failed)
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
        left.offer match 
          case l @ Emit(b, d) =>
            if cells.isEmpty then 
              left.state = l
              left.block
              live = false
              s
            else
              left.state = d.seekBranch
              broadcast(Sending(b, cells, Nil))
          case l @ Return(_) =>
            if cells.isEmpty then
              left.state = l
              live = false
              s
            else
              left.state = l
              broadcast(Closing(cells))
          case _ => s
      case s =>
        broadcast(s)

    val changed = right != state
    right = state
    changed

object BroadcastSynapse:
  def apply[Al, Bl <: Ar, Dl, Ar, Br, Dr](l: Cell[Al, Bl, Dl], r: Iterable[Cell[Ar, Br, Dr]]) = 
    new BroadcastSynapse:
      type A1 = Al
      type B1 = Bl
      type D1 = Dl
      type A2 = Ar
      type B2 = Br
      type D2 = Dr
      val left = l
      var right = Quiescent(r.toList)
      for d <- r do d.open
      left.subscribe