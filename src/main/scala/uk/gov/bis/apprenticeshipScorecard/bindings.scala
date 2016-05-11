package uk.gov.bis.apprenticeshipScorecard

import play.api.mvc.PathBindable

import scala.util.Try

object bindings {

  implicit val bdBinding = new PathBindable[BigDecimal] {
    override def bind(key: String, value: String): Either[String, BigDecimal] = {
      Try(BigDecimal(value)).toOption match {
        case Some(bd) => Right(bd)
        case None => Left(s"could not convert parameter $key to a number")

      }
    }

    override def unbind(key: String, value: BigDecimal): String = value.toString()
  }

}
