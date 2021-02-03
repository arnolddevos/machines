package machines

enum BroadcastState[A, B, C]:
  case Quiescent(cells: List[Cell[A, B, C]])
  case Sending(a: A, pending: List[Cell[A, B, C]], succeeded: List[Cell[A, B, C]])
  case Closing(pending: List[Cell[A, B, C]])

import BroadcastState._
import Machine._

abstract class BroadcastSynapse extends Synapse:
  type A1  
  type B1 <: A2
  type C1
  type A2
  type B2
  type C2
  val left: Cell[A1, B1, C1]
  var right: BroadcastState[A2, B2, C2]
  def live = right match 
    case Quiescent(Nil) => false
    case _ => true

  def sendToCell(cell: Cell[A2, B2, C2], state: BroadcastState[A2, B2, C2]): Boolean =
    val r1 = cell.state.seekReact
    r1 match 
      case React(s, f, r2) =>
        state match
          case Sending(a, _, _) => cell.accept(f, s, a)
          case Closing(_)       => cell.close(r2)
          case Quiescent(_)     =>
        true
      case Stop(x) =>
        cell.stop(x)
        true
      case Error(t) => throw t
      case _ => false

  def broadcast(s: BroadcastState[A2, B2, C2], failed: List[Cell[A2, B2, C2]]=Nil): BroadcastState[A2, B2, C2] =
    s match
      case Sending(a, c :: pending, succeeded) =>
        if sendToCell(c, s) then 
          broadcast(Sending(a, pending, c :: succeeded), failed)
        else
          broadcast(Sending(a, pending, succeeded), c :: failed)
      case Closing(c :: pending) =>
        if sendToCell(c, s) then 
          broadcast(Closing(pending), failed)
        else
          broadcast(Closing(pending), c :: failed)
      case Sending(a, Nil, succeeded) =>
          if failed.isEmpty then Quiescent(succeeded)
          else Sending(a, failed, succeeded)
      case Closing(Nil) =>
          if failed.isEmpty then Quiescent(Nil)
          else Closing(failed)    
      case s => s 

  def run: Boolean =
    val s = right match
      case s @ Quiescent(cells) =>
        val l1 = left.state.seekEmit
        l1 match 
          case Emit(b, l2) =>
            left.continue(l2)
            broadcast(Sending(b, cells, Nil))
          case Stop(x) =>
            left.stop(x)
            broadcast(Closing(cells))
          case Error(t) => throw t
          case _ => s
      case s =>
        broadcast(s)

    val changed = right != s
    right = s
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