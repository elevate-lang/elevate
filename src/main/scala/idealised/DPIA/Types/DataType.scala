package idealised.DPIA.Types

import idealised.DPIA.Nat
import idealised.{DPIA, SurfaceLanguage}
import lift.arithmetic.{ArithExpr, BigSum, RangeAdd}

import scala.language.implicitConversions

sealed trait DataType

sealed trait ComposedType extends DataType

sealed trait BasicType extends DataType

sealed trait ScalarType extends BasicType

object bool extends ScalarType { override def toString: String = "bool" }

object int extends ScalarType { override def toString: String = "int" }

object float extends ScalarType { override def toString: String = "float" }

object double extends ScalarType { override def toString: String = "double" }

object NatType extends ScalarType { override def toString: String = "nat" }

final case class IndexType(size: Nat) extends BasicType {
  override def toString: String = s"idx($size)"
}

final case class ArrayType(size: Nat, elemType: DataType) extends ComposedType {
  override def toString: String = s"$size.$elemType"
}

final case class DepArrayType private (size: Nat, elemFType: NatToData) extends ComposedType {

  override def toString: String = s"$size.$elemFType"

  override def equals(that: Any): Boolean = that match {
    case DepArrayType(size_, elemFType_) =>
      val eq = size == size_ && elemFType == elemFType_
      eq
    case _ => false
  }
}

object DepArrayType {
  def apply(size: Nat, f: DPIA.NatIdentifier => DataType): DepArrayType = {
    DepArrayType(size, NatToDataLambda(size, f))
  }
}

final case class RecordType(fst: DataType, snd: DataType) extends ComposedType {
  override def toString: String = s"($fst x $snd)"
}

sealed case class VectorType(size: Nat, elemType: ScalarType) extends BasicType {
  override def toString: String = s"<$size>$elemType"
}

object int2 extends VectorType(2, int)
object int3 extends VectorType(3, int)
object int4 extends VectorType(4, int)
object int8 extends VectorType(8, int)
object int16 extends VectorType(16, int)

object float2 extends VectorType(2, float)
object float3 extends VectorType(3, float)
object float4 extends VectorType(4, float)
object float8 extends VectorType(8, float)
object float16 extends VectorType(16, float)

final class NatToDataApply(val f: NatToData, val n: Nat) extends DataType {
  override def toString: String = s"$f($n)"
}

object NatToDataApply {
  def apply(f: NatToData, n: Nat): DataType = f match {
    case l: NatToDataLambda     => l.apply(n)
    case i: NatToDataIdentifier => new NatToDataApply(i, n)
  }

  def unapply(arg: NatToDataApply): Option[(NatToData, Nat)] = Some((arg.f, arg.n))
}

final case class DataTypeIdentifier(name: String) extends DataType with Kind.Identifier {
  override def toString: String = name
}

object ScalarType {
  implicit def apply(st: SurfaceLanguage.Types.ScalarType): ScalarType = {
    st match {
      case SurfaceLanguage.Types.NatType => NatType
      case SurfaceLanguage.Types.bool => bool
      case SurfaceLanguage.Types.int => int
      case SurfaceLanguage.Types.float => float
      case SurfaceLanguage.Types.double => double
    }
  }
}

object DataType {

  def substitute[T <: DataType](dt: DataType, `for`: DataType, in: T): T = {
    if (`for` == in) {
      dt.asInstanceOf[T]
    } else {
      (in match {
        case _: BasicType => in
        case a: ArrayType => ArrayType(a.size, substitute(dt, `for`, a.elemType))
        case r: RecordType => RecordType(substitute(dt, `for`, r.fst), substitute(dt, `for`, r.snd))
      }).asInstanceOf[T]
    }
  }

  def substitute[T <: DataType](ae: Nat, `for`: Nat, in: T): T = {
    (in match {
      case s: ScalarType => s
      case i: IndexType => IndexType(ArithExpr.substitute(i.size, Map((`for`, ae))))
      case a: ArrayType =>
        ArrayType(ArithExpr.substitute(a.size, Map((`for`, ae))),
          substitute(ae, `for`, a.elemType))
      case a: DepArrayType =>
        val subMap = Map((`for`,ae))
        val newSize = ArithExpr.substitute(a.size, subMap)
        val newElemFType = substitute(ae, `for`, a.elemFType)
        DepArrayType(newSize, newElemFType)
      case v: VectorType =>
        VectorType(ArithExpr.substitute(v.size, Map((`for`, ae))), v.elemType)
      case r: RecordType =>
        RecordType(substitute(ae, `for`, r.fst), substitute(ae, `for`, r.snd))
    }).asInstanceOf[T]
  }

  def substitute(ae: DPIA.Nat, `for`: DPIA.Nat, in: NatToData): NatToData = {
    in match {
      case i: NatToDataIdentifier => i
      case NatToDataLambda(x, body) => NatToDataLambda(x, substitute(ae, `for`, body))
    }
  }

  implicit def apply(dt: SurfaceLanguage.Types.DataType): DataType = {
    dt match {
      case bt: SurfaceLanguage.Types.BasicType => bt match {
        case st: SurfaceLanguage.Types.ScalarType => ScalarType(st)
        case vt: SurfaceLanguage.Types.VectorType => VectorType(vt.size, ScalarType(vt.elemType))
        case it: SurfaceLanguage.Types.IndexType => IndexType(it.size)
      }
      case ct: SurfaceLanguage.Types.ComposedType => ct match {
        case at: SurfaceLanguage.Types.ArrayType => ArrayType(at.size, DataType(at.elemType))
        case dat:SurfaceLanguage.Types.DepArrayType =>
          DepArrayType(dat.size, x => DataType.substitute(x, `for`= dat.elemType.x, in=DataType(dat.elemType.t)))
        case tt: SurfaceLanguage.Types.TupleType =>
          assert(tt.elemTypes.size == 2)
          //noinspection ZeroIndexToHead
          RecordType(DataType(tt.elemTypes(0)), DataType(tt.elemTypes(1)))
      }
      case i: SurfaceLanguage.Types.DataTypeIdentifier => DataTypeIdentifier(i.name)
    }
  }

  def getTotalNumberOfElements(dt: DataType): Nat = dt match {
    case _: BasicType => 1
    case _: RecordType => 1
    case a: ArrayType => getTotalNumberOfElements(a.elemType) * a.size
    case a: DepArrayType =>
      a.elemFType match {
        case NatToDataLambda(x, body) =>
          BigSum(from = 0, upTo = a.size - 1, `for` = x, `in` = getTotalNumberOfElements(body))
      }
    case _: DataTypeIdentifier => throw new Exception("This should not happen")
  }

  def getSize(dt: DataType): Nat = dt match {
    case _: IndexType | _: ScalarType => 1
    case _: RecordType => 1 // TODO: is this correct?
    case VectorType(size, _) => size
    case ArrayType(size, _) => size
    case DepArrayType(size, _) => size
    case _: DataTypeIdentifier => throw new Exception("This should not happen")
  }

  def getSizes(dt: DataType): Seq[Nat] = dt match {
    case ArrayType(size, elemType) => Seq(size) ++ getSizes(elemType)
    case DepArrayType(size, NatToDataLambda(_ , elemType)) => Seq(size) ++ getSizes(elemType) // TODO: is this correct?
    case _ => Seq(getSize(dt))
  }

  def getBaseDataType(dt: DataType): DataType = dt match {
    case _: BasicType => dt
    case _: RecordType => dt
    case _: DataTypeIdentifier => dt
    case ArrayType(_, elemType) => getBaseDataType(elemType)
    case DepArrayType(_, NatToDataLambda(_, elemType)) => getBaseDataType(elemType)
  }

  implicit class RecordTypeConstructor(dt1: DataType) {
    def x(dt2: DataType) = RecordType(dt1, dt2)
  }

  implicit class ArrayTypeConstructor(s: Nat) {
    def `.`(dt: DataType) = ArrayType(s, dt)
  }
}