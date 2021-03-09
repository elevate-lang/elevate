package FSmooth

export Type.Cases._
export ExpressionType.Cases._
export Num._

type Type = Type.Cases | ExpressionType
type ExpressionType = ExpressionType.Cases | Num

object Type:
  enum Cases:
    case TypeVar(name: String)
    case IncompleteFunType(inT: Type, outT: Type)
    case FunType(inT: Type, outT: ExpressionType)

    override def toString: String = this match
      case TypeVar(name) => name
      case IncompleteFunType(inT, outT) => s"($inT -> $outT)"
      case FunType(inT, outT) => s"($inT -> $outT)"

object ExpressionType:
  enum Cases:
    case ExpressionTypeVar(name: String)
    case Bool
    case Array(elemType: ExpressionType)
    case Pair(fst: ExpressionType, snd: ExpressionType)
  
    override def toString: String = this match
      case ExpressionTypeVar(name) => name
      case Bool => "Bool"
      case Array(elemType) => s"Array<$elemType>"
      case Pair(fst, snd) => s"$fst x $snd"

enum Num:
  case Double, Index, Card