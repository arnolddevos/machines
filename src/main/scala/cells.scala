package machines

import Machine._

enum Reaction:
  case Accepted, Rejected, Blocked
  
import Reaction._

class Cell[A, B, C](state0: Machine[A, B, C]):

  private var state_   = state0
  private var fanIn    = 0
  private var fanOut   = 0

  def state = state_

  private def blockOrRejectWith(basis: Machine[A, B, C]): Reaction =
    basis match 
      case Stop(_) | Error(_) => 
        state_ = basis
        Blocked
      case Emit(_, _) if fanOut == 0 => 
        state_ = basis
        Blocked
      case _ => Rejected

  def accept[A1 <: A](a: A1): Reaction =
    state.seekReact match 
      case React(s, f, _) => 
        state_ = f(s, a).seekBranch
        Accepted
      case s => 
        blockOrRejectWith(s)

  def close: Reaction =
    state.seekReact match 
      case React(_, _, c) => 
        fanIn -= 1
        if fanIn == 0 then state_ = c.seekBranch
        Accepted
      case s => 
        blockOrRejectWith(s)

  def continue(proposal: Machine[A, B, C]): Unit =
    state_ = proposal

  def block(proposal: Machine[A, B, C]): Unit =
    state_ = proposal
    fanOut -= 1
  
  def open: Unit =
    fanIn += 1

  def subscribe: Unit =
    fanOut += 1

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
    val proposal = left.state.seekEmit
    val reaction = 
      proposal match 
        case Emit(a, _) => right.accept(a)
        case Stop(_)    => right.close
        case _          => Rejected
  
    live = 
      reaction match 
        case Accepted =>
          proposal match 
            case Emit(_, c) =>
              left.continue(c.seekBranch)
              true
            case _ =>
              left.continue(proposal)
              false
  
        case Blocked => 
          left.block(proposal)
          false
  
        case Rejected => true
  
    reaction == Accepted || reaction == Blocked

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
      left.subscribe
      right.open


abstract class OneShotSynapse extends Synapse with CellPair:
  type C1 <: A2
  var live: Boolean = true
  var closing = false

  def run: Boolean =
    val proposal = left.state.seekEmitStop

    val reaction = 
      if closing then right.close
      else
        proposal match 
          case Stop(a) => right.accept(a)
          case _       => Rejected
  
    live = 
      reaction match 
        case Accepted =>
          if closing then false
          else
            left.continue(proposal)
            closing = true
            true 
  
        case Blocked => 
          left.block(proposal)
          false
  
        case Rejected => true
  
    reaction == Accepted || reaction == Blocked

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

