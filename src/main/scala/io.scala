package machines

enum IOResult[+A]:
  case Next(a: A)
  case Pending
  case End
  case Error(t: Throwable)

import IOResult._
import Outcome._

final class InputCell[+B](run: () => IOResult[B]) extends Cell[Nothing, B]:
  private var buffer: IOResult[B] = Pending
  def accept(a: Nothing): Outcome = Blocked 
  def open: Unit = ()
  def close: Outcome = Blocked
  def transferTo[B1 >: B](other: Cell[B1, Any]): Outcome =
    while buffer == Pending do buffer = run()
    buffer match
      case Next(a) => 
        if other.accept(a) == StateChange then
          buffer = Pending
          StateChange
        else Blocked
      case End =>
        if other.close == StateChange then FinalTransfer
        else Blocked
      case Pending => Blocked
      case Error(t) => throw t  

final class OutputCell[-A](run: Option[A] => IOResult[Unit]) extends Cell[A, Nothing]:
  var fanIn = 0
  def accept(a: A): Outcome =
    run(Some(a)) match 
      case Next(_) => StateChange
      case End => StateChange
      case Pending => Blocked
      case Error(t) => throw t
      
  def open: Unit = fanIn += 1
  def close: Outcome =
    if fanIn > 1 then 
      fanIn -= 1
      StateChange
    else
      run(None) match 
        case Next(_) => StateChange
        case End => StateChange
        case Pending => Blocked
        case Error(t) => throw t
  
  def transferTo[B1 >: Nothing](other: Cell[B1, Any]): Outcome = Blocked
