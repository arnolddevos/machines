package machines

import util.control.NonFatal
import Machine._

enum Reaction:
  case Accepted, Rejected, Blocked
  
import Reaction._

object Cell:
  def apply[A, B, C1 <: Pure, D](state0: Machine[A, B, C1, D], c: C1): Cell[A, B, D] =
    new Cell:
      type C = C1
      protected val capab: C = c
      protected var state_ = state0

abstract class Cell[A, B, D]:

  type C <: Pure
  type State = Machine[A, B, C, D]
  protected val capab: C
  protected var state_ : State

  final def state_=(next: State): Unit =
    state_ = next
    version_ += 1

  final def state = state_
  final def version = version_

  private var version_ = 0
  private var fanIn    = 0
  private var fanOut   = 0

  private def runEffect[S]( s: S, effect: (S, C) => State): Unit =
    state = 
      try
        effect(s, capab)
      catch
        case NonFatal(e) => Error(e)

  def accept[A1 <: A](a: A1): Reaction =

    def seek(s: State): Reaction = 
      s match 
        case React(x, f, _) => 
          state = f(x, a)
          Accepted
        case Return(_) | Error(_) | Stop => 
          state = s
          Blocked
        case Emit(_, _) => 
          if fanOut == 0 then 
            state = s
            Blocked
          else
            Rejected
        case Step(x, f) => 
          seek(f(x))
        case Effect(x, f) => 
            runEffect(x, f)
            seek(state)
        case s @ Branch(_, _) =>
          seek(s.seekReact)
    
    seek(state)

  def close: Reaction =

    def seek(s: State): Reaction = 
      s match 
        case React(_, _, d) => 
          fanIn -= 1
          if fanIn == 0 then state = d
          Accepted
        case Return(_) | Error(_) | Stop => 
          state = s
          Blocked
        case Emit(_, _) => 
          if fanOut == 0 then 
            state = s
            Blocked
          else
            Rejected
        case Step(x, f) => 
          seek(f(x))
        case Effect(x, f) =>   
            runEffect(x, f)
            seek(state)
        case s @ Branch(_, _) =>
          seek(s.seekReact)
    
    seek(state)
    
  def offer: State =

    def seek(s: State): State =
      s match
        case Step(x, f) =>
          seek(f(x))
        case Effect(x, f) => 
          runEffect(x, f)
          seek(state)
        case s @ Branch(_, _) =>
          seek(s.seekEmit)
        case Emit(_, _) | React(_, _, _) | Return(_) | Error(_) | Stop => s
  
    seek(state)

  def block: Unit =
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
  type D1
  type A2
  type B2
  type D2
  val left: Cell[A1, B1, D1]
  val right: Cell[A2, B2, D2]
   
abstract class CommonSynapse extends Synapse with CellPair:
  type B1 <: A2
  var live: Boolean = true
    
  def run: Boolean =
    val offer = left.offer
    val reaction = 
      offer match 
        case Emit(a, _) => right.accept(a)
        case Return(_)    => right.close
        case _          => Rejected
  
    live = 
      reaction match 
        case Accepted =>
          offer match 
            case Emit(_, d) =>
              left.state = d.seekBranch
              true
            case _ =>
              left.state = offer
              false

        case Blocked => 
          left.state = offer
          left.block
          false
  
        case Rejected => true
  
    reaction == Accepted || reaction == Blocked

object CommonSynapse:
  def apply[Al, Bl <: Ar, Dl, Ar, Br, Dr](l: Cell[Al, Bl, Dl], r: Cell[Ar, Br, Dr]) = 
    new CommonSynapse:
      type A1 = Al
      type B1 = Bl
      type D1 = Dl
      type A2 = Ar
      type B2 = Br
      type D2 = Dr
      val left = l
      val right = r
      left.subscribe
      right.open


abstract class OneShotSynapse extends Synapse with CellPair:
  type D1 <: A2
  var live: Boolean = true
  var closing = false

  def run: Boolean =
    val offer = left.offer

    val reaction = 
      if closing then right.close
      else
        offer match 
          case Return(a) => right.accept(a)
          case _       => Rejected
  
    live = 
      reaction match 
        case Accepted =>
          if closing then false
          else
            left.state = offer
            closing = true
            true 
  
        case Blocked => 
          left.state = offer
          left.block
          false
  
        case Rejected => true
  
    reaction == Accepted || reaction == Blocked

object OneShotSynapse:
  def apply[Al, Bl, Dl <: Ar, Ar, Br, Dr](l: Cell[Al, Bl, Dl], r: Cell[Ar, Br, Dr]) = 
    new OneShotSynapse:
      type A1 = Al
      type B1 = Bl
      type D1 = Dl
      type A2 = Ar
      type B2 = Br
      type D2 = Dr
      val left = l
      val right = r
      right.open

