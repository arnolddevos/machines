package machines

enum InputResult[+A]:
  case InputReceived(a: A)
  case InputClosed
  case InputError(t: Throwable)

enum OutputRequest[+A]:
  case SendOutput(a: A)
  case CloseOutput 

enum OutputResult:
  case OutputComplete
  case OutputError(t: Throwable)

