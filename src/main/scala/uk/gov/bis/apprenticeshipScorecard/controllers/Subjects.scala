package uk.gov.bis.apprenticeshipScorecard.controllers

import javax.inject.Inject

import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import uk.gov.bis.apprenticeshipScorecard.models.SubjectCode
import uk.gov.bis.apprenticeshipScorecard.tools.DataLoader

import scala.concurrent.ExecutionContext

class Subjects @Inject()(implicit ec: ExecutionContext) extends Controller with OptionResults {

  import DataLoader.dataStore

  def subject(subjectCode: SubjectCode) = Action { implicit request =>
    dataStore.subjects.get(subjectCode).map(Json.toJson(_)).toResult
  }
}
