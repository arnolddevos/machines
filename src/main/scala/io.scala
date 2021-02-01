package machines

import Machine._

enum InputResult[+A]:
  case InputReceived(a: A)
  case InputClosed
  case InputError(t: Throwable)

import InputResult._

enum OutputRequest[+A]:
  case SendOutput(a: A)
  case CloseOutput 

enum OutputResult:
  case OutputComplete
  case OutputError(t: Throwable)

abstract class Sensor extends Synapse:
  type A
  type B
  type C
  val effect: () => InputResult[A] 
  val cell: Cell[A, B, C]
  var live = true

  def fire: Boolean = if live then run else false

  def run: Boolean = 

    val r1 = cell.state.seekReact
    r1 match
      case React(s, f, r2) =>
        effect() match
          case InputReceived(a) =>
            cell.state = f(s, a).seekBranch
            true
          case InputClosed =>
            cell.fanIn -= 1
            if cell.fanIn == 0 then cell.state = r2.seekBranch
            live = false
            true
          case _ => false
      case _ => false


object Sensor:
  def apply[A1, B1, C1](e: () => InputResult[A1], c:  Cell[A1, B1, C1]) =
    new Sensor:
      type A = A1
      type B = B1
      type C = C1
      val effect = e
      val cell = c
      cell.fanIn += 1
