package elevate.core

sealed trait RewriteResult[P]:
  def getProgramOrElse(p: P): P
  def get: P

  def mapSuccess(f: P => P): RewriteResult[P]
  def flatMapSuccess(f: P => RewriteResult[P]): RewriteResult[P]

  def >>=(f: P => RewriteResult[P]): RewriteResult[P] = flatMapSuccess(f)

  def mapFailure(f: Strategy[P] => Strategy[P]): RewriteResult[P]
  def flatMapFailure(f: Strategy[P] => RewriteResult[P]): RewriteResult[P]

case class Success[P](p: P) extends RewriteResult[P]:
  Success.rewriteCount = Success.rewriteCount + 1
  override def getProgramOrElse(x: P): P = p
  override def get: P = p

  override def mapSuccess(f: P => P): RewriteResult[P] = Success(f(p))
  override def flatMapSuccess(f: P => RewriteResult[P]): RewriteResult[P] = f(p)

  override def mapFailure(f: Strategy[P] => Strategy[P]): RewriteResult[P] = this
  override def flatMapFailure(f: Strategy[P] => RewriteResult[P]): RewriteResult[P] = this

object Success:
  var rewriteCount = 1

case class Failure[P](s: Strategy[P]) extends RewriteResult[P]:
  override def getProgramOrElse(p: P): P = p
  override def get: P = throw NotApplicable(s)

  override def mapSuccess(f: P => P): RewriteResult[P] = this
  override def flatMapSuccess(f: P => RewriteResult[P]): RewriteResult[P] = this

  override def mapFailure(f: Strategy[P] => Strategy[P]): RewriteResult[P] = Failure(f(s))
  override def flatMapFailure(f: Strategy[P] => RewriteResult[P]): RewriteResult[P] = f(s)

case class NotApplicable[P](s: Strategy[P]) extends Exception
