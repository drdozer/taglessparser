package uk.co.turingatemyhamster.taglessparser

import scala.collection.immutable

object Lazy {
  type L[T] = () => T
  type nothing = () => Nothing
  val nothing: nothing = () => throw new IllegalStateException("Attempted to dereference nothing")
}

import Lazy.{L, nothing}


trait TypeSequence[R, S] {
  type T

  def apply(r: L[R], s: L[S]): L[T]
}

object TypeSequence {
  type Aux[R, S, T0] = TypeSequence[R, S] { type T = T0 }

  implicit object NothingLeftRight extends TypeSequence[Nothing, Nothing] {
    type T = Nothing

    def apply(r: nothing, s: nothing): nothing = nothing
  }

  implicit def NothingLeft[S]: TypeSequence.Aux[Nothing, S, S] = new TypeSequence[Nothing, S] {
    type T = S

    override def apply(r: nothing, s: L[S]): L[S] = s
  }


  implicit def NothingRight[R]: TypeSequence.Aux[R, Nothing, R] = new TypeSequence[R, Nothing] {
    type T = R

    override def apply(r: L[R], s: nothing): L[R] = r
  }

  implicit def LeftRight[R, S]: TypeSequence.Aux[R, S, (R, S)] = new TypeSequence[R, S] {
    type T = (R, S)

    override def apply(r: L[R], s: L[S]): L[(R, S)] = () => (r(), s())
  }
}


trait TypeAlternative[R, S] {
  type T

  def fromR(r: L[R]): L[T]
  def fromS(s: L[S]): L[T]
}

object TypeAlternative {
  type Aux[R, S, T0] = TypeAlternative[R, S] { type T = T0 }

  implicit object NothingLeftRight extends TypeAlternative[Nothing, Nothing] {
    type T = Nothing

    override def fromR(r: nothing): nothing = nothing

    override def fromS(s: nothing): nothing = nothing
  }

  implicit def NothingLeft[S]: TypeAlternative.Aux[Nothing, S, S] = new TypeAlternative[Nothing, S] {
    type T = S

    override def fromR(r: nothing): L[S] = nothing

    override def fromS(s: L[S]): L[S] = s
  }

  implicit def NothingRight[R]: TypeAlternative.Aux[R, Nothing, R] = new TypeAlternative[R, Nothing] {
    type T = R

    override def fromR(r: L[R]): L[R] = r

    override def fromS(s: nothing): L[R] = nothing
  }

  implicit def LeftRight[R, S, T0](implicit rt: R <:< T0, st: S <:< T0): TypeAlternative.Aux[R, S, T0] = new TypeAlternative[R, S] {
    type T = T0

    override def fromR(r: L[R]): L[T0] = () => r()

    override def fromS(s: L[S]): L[T0] = () => s()
  }
}


trait TypeOptional[T] {
  type To
  
  def none: L[To]
  def some(t: L[T]): L[To]
}

object TypeOptional {
  type Aux[T, To0] = TypeOptional[T] { type To = To0 }

  implicit object OptionalNothing extends TypeOptional[Nothing] {
    type To = Nothing

    override def none: nothing = nothing

    override def some(t: nothing): nothing = nothing
  }

  implicit def Optional[T]: TypeOptional.Aux[T, Option[T]] = new TypeOptional[T] {
    type To = Option[T]

    override def none: L[Option[T]] = () => None

    override def some(t: L[T]): L[Option[T]] = () => Some(t())
  }
}


trait TypeRep[T] {
  type Ts
  
  def empty: L[Ts]
  def extend(ts: L[Ts], t: L[T]): L[Ts]
}

object TypeRep {
  type Aux[T, Ts0] = TypeRep[T] { type Ts = Ts0 }
  
  implicit object RepNil extends TypeRep[Nothing] {
    type Ts = Nothing

    override def empty: nothing = nothing

    override def extend(ts: nothing, t: nothing): nothing = nothing
  }

  implicit def Rep[T]: TypeRep.Aux[T, immutable.Seq[T]] = new TypeRep[T] {
    type Ts = immutable.Seq[T]

    override def empty: L[immutable.Seq[T]] = () => immutable.Seq.empty

    override def extend(ts: L[immutable.Seq[T]], t: L[T]): L[immutable.Seq[T]] = () => ts() :+ t()
  }
}


object ast {

  sealed trait Parser[+T]

  object Accept extends Parser[Nothing]
  object Reject extends Parser[Nothing]

  case class MatchChar(c: Char) extends Parser[Nothing]
  case class MatchString(s: String) extends Parser[Nothing]

  case class Capture[S](p: Parser[S]) extends Parser[CharSequence]

  case class Map[S, T](p: Parser[S], f: S => T) extends Parser[T]
  case class MapOrFail[S, T](p: Parser[S], f: S => Either[String, T]) extends Parser[T]

  case class Sequence[R, S, T](r: Parser[R], s: Parser[S])(implicit T: TypeSequence.Aux[R, S, T]) extends Parser[T]
  case class Alternative[R, S, T](r: Parser[R], s: Parser[S])(implicit T: TypeAlternative.Aux[R, S, T]) extends Parser[T]
  case class Optional[T, To](p: Parser[T])(implicit To: TypeOptional.Aux[T, To]) extends Parser[To]
  case class Rep[T, Ts](p: Parser[T], min: Int, max: Int)(implicit Ts: TypeRep.Aux[T, Ts]) extends Parser[Ts]

}


import scala.language.higherKinds

trait TaglessParser[Rep[_]] {
  def accept: Rep[Nothing]
  def reject: Rep[Nothing]

  def matchChar(c: Char): Rep[Nothing]
  def matchString(s: String): Rep[Nothing]

  def capture[S](p: Rep[S]): Rep[CharSequence]

  def map[S, T](p: Rep[S], f: S => T): Rep[T]
  def mapOrFail[S, T](p: Rep[S], f: S => Either[String, T]): Rep[T]

  def sequence[R, S, T](r: Rep[R], s: Rep[S])(implicit T: TypeSequence.Aux[R, S, T]): Rep[T]
  def alternative[R, S, T](r: Rep[R], s: Rep[S])(implicit T: TypeAlternative.Aux[R, S, T]): Rep[T]
  def optional[T, To](p: Rep[T])(implicit To: TypeOptional.Aux[T, To]): Rep[To]
  def rep[T, Ts](p: Rep[T], min: Int, max: Int)(implicit Ts: TypeRep.Aux[T, Ts]): Rep[Ts]
}

