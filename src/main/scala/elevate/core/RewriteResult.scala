package elevate.core

enum RewriteResult[P]:
  case Success(p: P)
  case Failure(s: Strategy[P])

  def get: P = this match
    case Success(p) => p
    case Failure(s) => throw NotApplicable(s)

  def getProgramOrElse(p: P): P = this match
    case Success(p) => p
    case Failure(_) => p

  def mapSuccess(f: P => P): RewriteResult[P] = this match
    case Success(p) => Success(f(p))
    case s@Failure(_) => s 

  def flatMapSuccess(f: P => RewriteResult[P]): RewriteResult[P] = this match
    case Success(p) => f(p)
    case s@Failure(_) => s

  def >>=(f: P => RewriteResult[P]): RewriteResult[P] = flatMapSuccess(f)

  def mapFailure(f: Strategy[P] => Strategy[P]): RewriteResult[P] = this match
    case s@Success(p) => s
    case Failure(s) => Failure(f(s))

  def flatMapFailure(f: Strategy[P] => RewriteResult[P]): RewriteResult[P] = this match
    case s@Success(p) => s
    case Failure(s) => f(s)

case class NotApplicable[P](s: Strategy[P]) extends Exception
