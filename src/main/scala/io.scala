package machines

import Machine._

enum InputResult[+A]:
  case InputReceived(a: A)
  case InputClosed
  case InputError(t: Throwable)

import InputResult._

enum InputRequest:
  case ReadInput, CloseInput

import InputRequest._

enum OutputRequest[+A]:
  case SendOutput(a: A)
  case CloseOutput 

import OutputRequest._

enum OutputResult:
  case OutputComplete
  case OutputError(t: Throwable)

import OutputResult._

trait CellRef:
  type A
  type B
  type C
  val cell: Cell[A, B, C]

abstract class Sensor extends Synapse with CellRef:
  val effect: InputRequest => InputResult[A] 
  var live = true

  def run: Boolean = false
    

object Sensor:
  def apply[A1, B1, C1](e: InputRequest => InputResult[A1], c:  Cell[A1, B1, C1]) =
    new Sensor:
      type A = A1
      type B = B1
      type C = C1
      val effect = e
      val cell = c
      cell.open

abstract class Motor extends Synapse with CellRef:
  val effect: OutputRequest[B] => OutputResult 
  var live = true

  def run: Boolean = 

    cell.state.seekEmit match
      case Emit(b, l2) =>
        effect(SendOutput(b)) match
          case OutputComplete =>
            cell.continue(l2.seekBranch)
            true
          case OutputError(t) => throw t
      case s @ Stop(_) =>
        effect(CloseOutput) match
          case OutputComplete => 
            cell.continue(s)
            live = false
            true
          case OutputError(t) => throw t
      case _ => false

object Motor:
  def apply[A1, B1, C1](e: OutputRequest[B1] => OutputResult, c:  Cell[A1, B1, C1]) =
    new Motor:
      type A = A1
      type B = B1
      type C = C1
      val effect = e
      val cell = c
      cell.subscribe
