package zombie.tvirus.parser

enum Op:
    case PLUS, MINUS, MULT

enum Type:
    case Int()
    case Func(from: Type, target: Type)
    case DataType(name: String)

enum Expr:
    case Var(id: String)
    case Int(int: Integer)
    case Calc(l: Expr, op: Op, r: Expr)
    case App(f: Expr, x: Expr)
    case Abs(x: String, body: Expr)

enum Statement:
    case Let(name: String, expr: Expr)
    case Data(name: String, unionFields: List[(String, List[Type])])