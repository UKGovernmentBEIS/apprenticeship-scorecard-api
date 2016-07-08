package uk.gov.bis.apprenticeshipScorecard.tools

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.std.list._
import cats.syntax.validated._
import cats.{Applicative, Semigroup, SemigroupK}
import eu.timepit.refined._
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.boolean.{And, Or}

import scala.util.Try

object FieldExtractors {

  trait Read[T] {
    def read(s: String): ValidatedNel[String, T]
  }

  def mandatory[T: Read](fieldName: String)(implicit fields: Map[String, String]): ValidatedNel[String, T] = {
    fields.get(fieldName) match {
      case None => Invalid(NonEmptyList(s"no field found with name $fieldName"))
      case Some(s) if s.trim() == "" => Invalid(NonEmptyList(s"mandatory field $fieldName is blank"))
      case Some(s) => implicitly[Read[T]].read(s).leftMap(_.map(e => s"field name: $fieldName - $e"))
    }
  }

  def optional[T: Read](fieldName: String)(implicit fields: Map[String, String]): ValidatedNel[String, Option[T]] = {
    fields.get(fieldName) match {
      case None => Invalid(NonEmptyList(s"no field found with name $fieldName"))
      case Some(s) if s.trim() == "" => Valid(None)
      case Some(s) => implicitly[Read[T]].read(s).leftMap(_.map(e => s"field name: $fieldName - $e")).map(Some(_))
    }
  }

  /**
    * Provide a default value in the case where the field is invalid for any reason. In other words,
    * converts an Invalid result to a Valid result using the supplied value.
    */
  implicit class Default[T](v: ValidatedNel[String, T]) {
    def default(t: T): ValidatedNel[String, T] = v match {
      case Valid(_) => v
      case Invalid(_) => Valid(t)
    }
  }

  /**
    * Converts a value of type T with a refinement predicate P. There needs to be a Validate[T, P] instance available.
    */
  implicit def refinedConverter[T: Read, P](implicit v: Validate[T, P]): Read[T Refined P] = new Read[T Refined P] {
    override def read(s: String): ValidatedNel[String, Refined[T, P]] = {
      implicitly[Read[T]].read(s).andThen { value =>
        refineV[P](value) match {
          case Left(err) => err.invalidNel
          case Right(r) => r.validNel
        }
      }
    }
  }

  implicit val stringConverter = new Read[String] {
    @scala.annotation.tailrec
    def trimQuotes(s: String): String = {
      s match {
        case _ if s.startsWith("\"") => trimQuotes(s.drop(1))
        case _ if s.endsWith("\"") => trimQuotes(s.dropRight(1))
        case _ => s
      }
    }

    override def read(s: String): ValidatedNel[String, String] = Valid(trimQuotes(s))
  }

  implicit val bigDecimalConverter = new Read[BigDecimal] {
    override def read(s: String): ValidatedNel[String, BigDecimal] = {
      Try(BigDecimal(s))
        .map(Valid(_))
        .getOrElse(Invalid(NonEmptyList(s"could not convert '$s' to a number")))
    }
  }

  implicit val intConverter = new Read[Int] {
    override def read(s: String): ValidatedNel[String, Int] = {
      Try(s.toInt)
        .map(Valid(_))
        .getOrElse(Invalid(NonEmptyList(s"could not convert '$s' to an Int")))
    }
  }

  implicit val longConverter = new Read[Long] {
    override def read(s: String): ValidatedNel[String, Long] = {
      Try(s.toLong)
        .map(Valid(_))
        .getOrElse(Invalid(NonEmptyList(s"could not convert '$s' to a Long")))
    }
  }


  // cats boilerplate to get Applicative syntax out of ValidatedNel
  implicit def validatedApplicative[E: Semigroup]: Applicative[Validated[E, ?]] =
    new Applicative[Validated[E, ?]] {
      def ap[A, B](f: Validated[E, A => B])(fa: Validated[E, A]): Validated[E, B] =
        (fa, f) match {
          case (Valid(a), Valid(fab)) => Valid(fab(a))
          case (i@Invalid(_), Valid(_)) => i
          case (Valid(_), i@Invalid(_)) => i
          case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
        }

      def pure[A](x: A): Validated[E, A] = Validated.valid(x)
    }

  implicit def nelSemigroup[T]: Semigroup[NonEmptyList[T]] =
    SemigroupK[NonEmptyList].algebra[T]

}
