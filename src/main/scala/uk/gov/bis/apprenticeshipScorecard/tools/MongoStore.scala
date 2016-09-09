package uk.gov.bis.apprenticeshipScorecard.tools

import play.api.Logger
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.{DefaultDB, MongoConnection, MongoDriver}
import reactivemongo.bson.{BSONDocument, BSONDocumentReader, BSONDouble, BSONReader, Macros}
import uk.gov.bis.apprenticeshipScorecard.models._

import scala.concurrent.{ExecutionContext, Future}

object MongoStore {

  val mongoUri = Option(System.getenv("MONGODB_URL")).getOrElse("mongodb://localhost:27017/apprenticeship-scorecard")

  import ExecutionContext.Implicits.global

  // Connect to the database: Must be done only once per application
  val driver: MongoDriver = MongoDriver()
  val uriF = Future.fromTry(MongoConnection.parseURI(mongoUri))
  val connF = uriF.map(driver.connection)

  lazy val db: Future[DefaultDB] = for {
    uri <- uriF
    conn <- connF
    dn <- Future(uri.db.getOrElse("apprenticeship-scorecard"))
    db <- conn.database(dn)
  } yield db

  def providers: Future[BSONCollection] = db.map(_.collection("provider"))

  def apprenticeships: Future[BSONCollection] = db.map(_.collection("apprenticeship"))

  implicit def ukprnReader = Macros.reader[UKPRN]

  implicit val bdReader: BSONReader[BSONDouble, BigDecimal] = new BSONReader[BSONDouble, BigDecimal] {
    override def read(bson: BSONDouble): BigDecimal = BigDecimal(bson.value)
  }

  implicit def addressReader = Macros.reader[Address]

  implicit def scReader = Macros.reader[SubjectCode]

  implicit def lstatsReader = Macros.reader[LearnerStats]

  implicit def qstatsReader = Macros.reader[QualificationStats]

  implicit def earningsReader = Macros.reader[Earnings]

  implicit def providerReader: BSONDocumentReader[Provider] = Macros.reader[Provider]

  implicit def apprenticeshipReader: BSONDocumentReader[Apprenticeship] = Macros.reader[Apprenticeship]

  def readProviders: Future[List[Provider]] = providers.flatMap(_.find(BSONDocument()).cursor[Provider]().collect[List]())

  def readApprenticeships: Future[List[Apprenticeship]] = apprenticeships.flatMap(_.find(BSONDocument()).cursor[Apprenticeship]().collect[List]())

  def shutdown(): Future[Unit] = {
    connF.map { c =>
      c.close()
      c.actorSystem.terminate()
    }
  }
}
