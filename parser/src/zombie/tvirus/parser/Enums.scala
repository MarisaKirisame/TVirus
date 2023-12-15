package zombie.tvirus.parser

enum Op:
    case PLUS, MINUS, MULT

enum Expr:
    case Var(id: String)
    case Int(int: Integer)
    case Calc(l: Expr, op: Op, r: Expr)
    case App(f: Expr, x: Expr)
    case Abs(x: String, body: Expr)
    case AbsRec(f: String, x: String, body: Expr)
