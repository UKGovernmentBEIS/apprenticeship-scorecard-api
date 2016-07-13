package uk.gov.bis.apprenticeshipScorecard.controllers

import play.api.http.Writeable
import play.api.mvc.{Controller, Result}

trait OptionResults {
  self: Controller =>

  implicit class ResultSyntax[T: Writeable](o: Option[T]) {
    def toResult: Result = o match {
      case None => NotFound
      case Some(t) => Ok(t)
    }
  }

}
