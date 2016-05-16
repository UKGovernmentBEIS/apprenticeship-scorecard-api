package uk.gov.bis.apprenticeshipScorecard.controllers

import javax.inject.Inject

import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import uk.gov.bis.apprenticeshipScorecard.models.SubjectCode
import uk.gov.bis.apprenticeshipScorecard.tools.TSVLoader

import scala.concurrent.ExecutionContext

class Subjects @Inject()(implicit ec: ExecutionContext) extends Controller {

  import TSVLoader.dataStore

  def subject(subjectCode: SubjectCode) = Action { implicit request =>
    dataStore.subjects.get(subjectCode) match {
      case None => NotFound
      case Some(p) => Ok(Json.toJson(p))
    }
  }
}
