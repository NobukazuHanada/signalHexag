package hexasignal.pico


sealed abstract class Result[+T] {
  def map[U](f: T => U): Result[U]

  def flatMap[U](f: T => Result[U]): Result[U] =
  this match {
    case Success(t) => f(t)
    case error:Error => error
  }

  def withFilter(p:T=>Boolean) : Result[T] =
    this match {
      case result@Success(t) =>
        if(p(t)) result else FilterError
      case error => error
    }
}

object Result{
  implicit class ToResult[+T](t:T) {
    def toResult : Result[T] = Success(t)
  }


  implicit class ToResultSeq[+T](seqResult: Seq[Result[T]]) {
    def toResultSeq : Result[Seq[T]] =
    seqResult.foldRight(Seq().toResult:Result[Seq[T]]) {
      case (Success(elm), Success(lst)) => Success(elm +: lst)
      case (error:Error, Success(_)) => error
      case (Success(_), error:Error) => error
      case (error:Error, error2:Error) => error
    }
  }
}

case class Success[+T](result: T) extends Result[T] {
  def map[U](f:T=>U):Result[U] =
    this match {
      case Success(result) => Success(f(result))
    }
}

sealed abstract class Error extends Result[Nothing] {
  def map[U](f:Nothing=>U):Result[Nothing] = this
}

case class ReadError(msg:String) extends Error
case class ParseError(msg:String) extends Error
case class VMError(msg:String) extends Error
case object FilterError extends Error
