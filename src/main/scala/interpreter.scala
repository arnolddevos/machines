package machines

import scala.collection.mutable

class MultiMap[K[_], V[_]](underlying: mutable.Map[Any, mutable.Buffer[Any]]):
  def get[A](k: K[A]): Iterable[V[A]] =
    underlying.get(k).fold(Iterable.empty[V[A]])(_.asInstanceOf[Iterable[V[A]]])

class MapBuilder[K[_], V[_]]:
  val underlying = mutable.Map[Any, mutable.Buffer[Any]]()

  def add[A](k: K[A], v: V[A]): Unit =
    if underlying.contains(k) then underlying(k).append(v)
    else underlying(k) = mutable.Buffer(v)

  def result = MultiMap[K, V](underlying)

import Machine._

enum Outcome:
  case Blocked, StateChange, FinalTransfer
import Outcome._

enum Direction:
  case GoLeft, GoRight, NoBranch    
import Direction._

trait Cell[-A, +B]:
  def accept(a: A): Outcome
  def close: Outcome
  def transferTo[B1 >: B](other: Cell[B1, Any]): Outcome

class MachineCell[-A, +B](s: Machine[A, B, Any], fi: Int) extends Cell[A, B]:
  private type State = Either[InnerCell[A, B], InnerCell[Nothing, B]]
  private var state: State = Left(InnerCell(s))
  private var fanIn: Int = fi

  def accept(a: A): Outcome =
    state match 
      case Left(c)  => c.accept(a)
      case Right(_) => Blocked

  def close: Outcome =
    if fanIn > 1 then
      fanIn -= 1
      StateChange
    else
      state match
        case Left(c) =>
          c.close match
            case Some(s) =>
              state = Right(s) 
              StateChange
            case None => Blocked
        case Right(_) => Blocked

  def transferTo[B1 >: B](other: Cell[B1, Any]): Outcome =
    state match 
      case Left(c)  => c.transferTo(other)
      case Right(c) => c.transferTo(other)

class InnerCell[-A, +B](s: Machine[A, B, Any]):
  private var state: Machine[A, B, Any] = s

  def run[A, B](m: Machine[A, B, Any], d: Direction): Machine[A, B, Any] =
    def attempt(m: Machine[A, B, Any]): Machine[A, B, Any] =
      m match
        case Branch(_, c) if d == GoRight => attempt(c)
        case Branch(c, _) if d == GoLeft => attempt(c)
        case Defer(s, f)  => attempt(f(s))
        case m => m
    attempt(m)

  def accept(a: A): Outcome =  
    val m = run(state, GoLeft)
    m match
      case React(s, f, _) =>
        state = run(f(s, a), NoBranch)
        StateChange
      case _ => Blocked

  def close: Option[InnerCell[Nothing, B]] =
    val m = run(state, GoLeft)
    m match
      case React(_, _, c) => Some(InnerCell(run(c, NoBranch)))
      case _ => None

  def transferTo[B1 >: B](other: Cell[B1, Any]): Outcome =
    val m = run(state, GoRight)
    m match
      case Emit(b, c) =>
        if other.accept(b) == StateChange then
          state = run(m, NoBranch)
          StateChange
        else Blocked
      case Stop(_) =>
        if other.close == StateChange then
          state = m
          FinalTransfer
        else Blocked
      case _ => Blocked

abstract class Synapse:
  type A
  val left: Cell[Nothing, A]
  val right: Cell[A, Any]
  var live: Boolean

  def fire: Boolean =
    if live then
      left.transferTo(right) match
        case StateChange => true
        case Blocked => false
        case FinalTransfer =>
          live = false
          true
    else false

object Synapse:
  def apply[A1](l: Cell[Nothing, A1], r: Cell[A1, Any]) = 
    new Synapse:
      type A = A1
      val left = l
      val right = r
      var live = false

def run(syns: Array[Synapse]): Unit =

  def cycle: Unit =
    var live = false
    for syn <- syns
    do live |= syn.fire
    if live then cycle

  cycle

