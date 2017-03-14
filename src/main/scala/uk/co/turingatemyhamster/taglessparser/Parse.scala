package uk.co.turingatemyhamster.taglessparser

/**
  * Created by nmrp3 on 13/03/17.
  */
sealed trait Parse[+T] {
  def within: CharSequence
  def at: Int
}

case class Success[T](within: CharSequence, at: Int, t: T) extends Parse[T]
case class Failure(within: CharSequence, at: Int) extends Parse[Nothing]

trait TaglessParse[Rep[_]] {
  def start(within: CharSequence): Rep[Nothing]

  def success[T](at: Rep[T]): Rep[T]
  def failure[T](at: Rep[T], reason: String): Rep[Nothing]

  def extendSuccess[T](from: Rep[T], by: Int): Rep[T]
  def mapSuccess[S, T](from: Rep[S], f: S => T): Rep[T]
}

object Parse {

  type PParse[T] = Parse[T] => Parse[T]

  implicit object parseTagless extends TaglessParser[PParse] {
    override def accept: PParse[Nothing] = p => p

    override def reject: PParse[Nothing] = {
      case Success(within, at, _) =>
        Failure(within, at)
      case f : Failure =>
        f
    }

    override def matchChar(c: Char): PParse[Nothing] = {
      case Success(within, at, t) if within.charAt(at) == c =>
        Success(within, at + 1, t)
      case Success(within, at, _) =>
        Failure(within, at)
      case f : Failure =>
        f
    }

    override def matchString(s: String): PParse[Nothing] = {
      case Success(within, at, t) if within.length < at + s.length && within.subSequence(at, at + s.length) == s =>
        Success(within, at + s.length, t)
      case Success(within, at, _) =>
        Failure(within, at)
      case f : Failure =>
        f
    }

    override def capture[S](p: PParse[S]): PParse[CharSequence] = {
      case s@Success(within, at, _) => p(s) match {
        case t@Success(_, until, _) => Success(within, until, within.subSequence(at, until))
        case f : Failure => f
      }
      case f : Failure => f
    }


    override def map[S, T](p: PParse[S], f: (S) => T): PParse[T] = { a =>
      p(a) match {
        case Success(within, at, s) => Success(within, at, f(s))
        case f : Failure => f
      }
    }

    override def mapOrFail[S, T](p: PParse[S], f: (S) => Either[String, T]): PParse[T] = ???

    override def sequence[R, S, T](r: PParse[R], s: PParse[S])(implicit T: TypeSequence.Aux[R, S, T]): PParse[T] = ???

    override def alternative[R, S, T](r: PParse[R], s: PParse[S])(implicit T: TypeAlternative.Aux[R, S, T]): PParse[T] = ???

    override def optional[T, To](p: PParse[T])(implicit To: TypeOptional.Aux[T, To]): PParse[To] = ???

    override def rep[T, Ts](p: PParse[T], min: Int, max: Int)(implicit Ts: TypeRep.Aux[T, Ts]): PParse[Ts] = ???
  }

}