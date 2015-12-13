import PhraseType._

import scala.language.implicitConversions

object PhraseExtensions {
  implicit class BinOps(lhs: Phrase[ExpType]) {
    def +(rhs: Phrase[ExpType]) = BinOp(BinOp.Op.ADD, lhs, rhs)
    def -(rhs: Phrase[ExpType]) = BinOp(BinOp.Op.SUB, lhs, rhs)
    def *(rhs: Phrase[ExpType]) = BinOp(BinOp.Op.MUL, lhs, rhs)
    def /(rhs: Phrase[ExpType]) = BinOp(BinOp.Op.DIV, lhs, rhs)
    def %(rhs: Phrase[ExpType]) = BinOp(BinOp.Op.MOD, lhs, rhs)
  }

  implicit class CallLambda[T1 <: PhraseType, T2 <: PhraseType](lambda: Lambda[T1, T2]) {
    def apply(arg: Phrase[T1]) = Apply(lambda, arg)
  }

  implicit class SequentialComposition(c1: Phrase[CommandType]) {
    def `;`(c2: Phrase[CommandType]) = Seq(c1, c2)
  }

  implicit class Assignment(lhs: Phrase[AccType]) {
    def :=(rhs: Phrase[ExpType]) = Assign(lhs, rhs)
  }

  implicit class PairTypeConstructor[T1 <: PhraseType](t1: T1) {
    def x[T2 <: PhraseType](t2: T2) = PairType(t1, t2)
  }

  implicit class FunctionTypeConstructor[T1 <: PhraseType](t1: T1) {
    def ->[T2 <: PhraseType](t2: T2) = FunctionType(t1, t2)
  }

  implicit class PassiveFunctionTypeConstructor[T1 <: PhraseType](t1: T1) {
    def `->p`[T2 <: PhraseType](t2: T2) = PassiveFunctionType(t1, t2)
  }

  implicit def toPair[T1 <: PhraseType,
                      T2 <: PhraseType](pair: (Phrase[T1], Phrase[T2])): Pair[T1, T2] = {
    Pair(pair._1, pair._2)
  }

  implicit def toLiteral(i: Int): IntLiteral = IntLiteral(i)
}

import PhraseExtensions._

object VarType {
  def apply(dataType: DataType) = ExpType(dataType) x AccType(dataType)
}

object `;` {
  def apply() = {
    //: Phrase[CommandType x CommandType -> CommandType]
    \ ( CommandType() x CommandType() ) {
      pair => Seq(Proj1(pair), Proj2(pair))
    }
  }
}

object makeNew {
  // TODO: make passive lambda ...
  def apply(t: DataType) = {
    //: Phrase[(VarType -> CommandType) -> CommandType]
    \ ( VarType(t) -> CommandType() ) {
      f => New(f)
    }
  }
}

object := {
  // TODO: add passivity
  def apply(t: DataType) = {
    //: Phrase[ AccType x ExpType -> CommandType ]
    \ ( AccType(t) x ExpType(t) ) {
      pair => Assign(π1(pair), π2(pair))
    }
  }
}

object makeIfThenElse {
  // TODO: add passivity
  def apply[T <: PhraseType](t: T) = {
    //: Phrase[ ExpType x T x T -> T ]
    \ ( ExpType(bool) x t x t ) {
      args => {
        val firstTwo = π1(args)
        val cond  = π1(firstTwo)
        val thenP = π2(firstTwo)
        val elseP = π2(args)
        IfThenElse(cond, thenP, elseP)
      }
    }
  }
}

object `if` {
  def apply[T <: PhraseType](cond: Phrase[ExpType], thenP: Phrase[T], elseP: Phrase[T]) = {
    IfThenElse(cond, thenP, elseP)
  }
}

object makeFor {
  def apply() =  {
    //: Phrase[ ExpType x (ExpType -> CommandType) -> CommandType ]
    \ ( ExpType(int) x (ExpType(int) -> CommandType()) ) {
      args => {
        For(π1(args), π2(args))
      }
    }
  }
}

object π1 {
  def apply[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2]) = Proj1(pair)
}

object π2 {
  def apply[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2]) = Proj2(pair)
}

object identifier {
  def apply(name: String) = Ident[ExpType](name)

  def apply[T <: PhraseType](name: String, t: T) = {
    val i = Ident[T](name)
    i.t = t
    i
  }
}

trait funDef {
  var counter = 0

  def newName(): String = {
    counter += 1
    "v" + counter
  }

  def apply[T1 <: PhraseType, T2 <: PhraseType](f: Ident[T1] => Phrase[T2]): Lambda[T1, T2] = {
    val param = Ident[T1]( newName() )
    Lambda(param, f(param))
  }

  def apply[T <: PhraseType](t: ExpType)(f: Ident[ExpType] => Phrase[T]): Lambda[ExpType, T] = {
    val param = identifier(newName(), t)
    Lambda(param, f(param))
  }

  def apply[T <: PhraseType](t: AccType)(f: Ident[AccType] => Phrase[T]): Lambda[AccType, T] = {
    val param = identifier(newName(), t)
    Lambda(param, f(param))
  }

  def apply[T1 <: PhraseType,
            T2 <: PhraseType,
            T3 <: PhraseType](t: T1 x T2)
                             (f: Pair[T1, T2] => Phrase[T3]): Lambda[T1 x T2, T3] = {
    val n = newName()
    val param = Pair(identifier(n, t.t1), identifier(n, t.t2))
    Lambda(param, f(param))
  }

  def apply[T1 <: PhraseType,
            T2 <: PhraseType,
            T3 <: PhraseType](t: T1 -> T2)
                             (f: Ident[T1 -> T2] => Phrase[T3]): Lambda[T1 -> T2, T3] = {
    val param = identifier(newName(), t)
    Lambda(param, f(param))
  }

  def apply[T1 <: PhraseType, T2 <: PhraseType](param : Phrase[T1],
                                                body : Phrase[T2]): Lambda[T1, T2] = {
    Lambda(param, body)
  }
}
object fun extends funDef
object \ extends funDef
object λ extends funDef

object skip extends SkipPhrase

