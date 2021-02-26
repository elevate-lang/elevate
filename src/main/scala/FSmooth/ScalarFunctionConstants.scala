package FSmooth

import FSmooth.DSL._

//noinspection DuplicatedCode
object ScalarFunctionConstants:
  case class `+`(override val t: Type) extends Constants(t):
    override def typeScheme: Type = implNum(Num => Num -> Num -> Num )
    override def setType(t: Type): Constants = new `+`(t)

  case class `-`(override val t: Type) extends Constants(t):
    override def typeScheme: Type = implNum(Num => Num -> Num -> Num )
    override def setType(t: Type): Constants = new `-`(t)

  case class `*`(override val t: Type) extends Constants(t):
    override def typeScheme: Type = implNum(Num => Num -> Num -> Num )
    override def setType(t: Type): Constants = `*`(t)

  case class `/`(override val t: Type) extends Constants(t):
    override def typeScheme: Type = implNum(Num => Num -> Num -> Num )
    override def setType(t: Type): Constants = `/`(t)

  case class `**`(override val t: Type) extends Constants(t):
    override def typeScheme: Type = implNum(Num => Num -> Num -> Num )
    override def setType(t: Type): Constants = `**`(t)

  case class sign(override val t: Type) extends Constants(t):
    override def typeScheme: Type = implNum(Num => Num -> Num )
    override def setType(t: Type): Constants = sign(t)

  case class cos(override val t: Type) extends Constants(t):
    override def typeScheme: Type = implNum(Num => Num -> Num )
    override def setType(t: Type): Constants = cos(t)

  case class tan(override val t: Type) extends Constants(t):
    override def typeScheme: Type = implNum(Num => Num -> Num )
    override def setType(t: Type): Constants = tan(t)

  case class log(override val t: Type) extends Constants(t):
    override def typeScheme: Type = implNum(Num => Num -> Num )
    override def setType(t: Type): Constants = log(t)

  case class exp(override val t: Type) extends Constants(t):
    override def typeScheme: Type = implNum(Num => Num -> Num )
    override def setType(t: Type): Constants = exp(t)

  case class sqrt(override val t: Type) extends Constants(t):
    override def typeScheme: Type = implNum(Num => Num -> Num )
    override def setType(t: Type): Constants = sqrt(t)

  case class `>`(override val t: Type) extends Constants(t):
    override def typeScheme: Type = implNum(Num => (Num -> Num) -> Bool )
    override def setType(t: Type): Constants = `>`(t)

  case class `<`(override val t: Type) extends Constants(t):
    override def typeScheme: Type = implNum(Num => (Num -> Num) -> Bool )
    override def setType(t: Type): Constants = `<`(t)

  case class `=:=`(override val t: Type) extends Constants(t):
    override def typeScheme: Type = implNum(Num => (Num -> Num) -> Bool )
    override def setType(t: Type): Constants = `=:=`(t)

  case class `<>`(override val t: Type) extends Constants(t):
    override def typeScheme: Type = implNum(Num => (Num -> Num) -> Bool )
    override def setType(t: Type): Constants = `<>`(t)

  case class `&&`(override val t: Type) extends Constants(t):
    override def typeScheme: Type = implNum(Num => (Num -> Num) -> Bool )
    override def setType(t: Type): Constants = `&&`(t)

  case class `||`(override val t: Type) extends Constants(t):
    override def typeScheme: Type = implNum(Num => (Num -> Num) -> Bool )
    override def setType(t: Type): Constants = `||`(t)

  case class `!`(override val t: Type) extends Constants(t):
    override def typeScheme: Type = Bool -> Bool
    override def setType(t: Type): Constants = new `!`(t)
end ScalarFunctionConstants
