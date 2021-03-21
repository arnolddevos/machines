package machines

enum Op:
  case Emit[X, A1, D1, B2, D2](left: Cell[A1, X, D1], right: Cell[X, B2, D2])
  case Return[X, A1, B1, B2, D2](left: Cell[A1, B1, X], right: Cell[X, B2, D2])
  case Error[A1, B1, D1, B2, D2](left: Cell[A1, B1, D1], right: Cell[Throwable, B2, D2])
  case Run[A, B, D](cell: Cell[A, B, D])
