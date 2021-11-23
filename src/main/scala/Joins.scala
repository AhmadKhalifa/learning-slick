package com.slick.learning

import slick.lifted.ProvenShape

import slick.dbio.Effect
import slick.driver.H2Driver.api._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

object Joins extends App {

  case class Artist(name: String, id: Long = 0L)
  class ArtistTable(tag: Tag) extends Table[Artist](tag, "artists") {
    def name: Rep[String] = column[String]("name")

    def id: Rep[Long] = column[Long]("id", O.PrimaryKey, O.AutoInc)

    override def * : ProvenShape[Artist] = (name, id) <> (Artist.tupled, Artist.unapply)
  }
  lazy val artists = TableQuery[ArtistTable]

  // ==================================

  case class Album(artistId: Long, title: String, year: Int, rating: Rating, id: Long = 0L)
  class AlbumTable(tag: Tag) extends Table[Album](tag, "albums") {
    def artistId: Rep[Long] = column[Long]("artistId")

    def title: Rep[String] = column[String]("title")

    def year: Rep[Int] = column[Int]("year")

    def rating: Rep[Rating] = column[Rating]("rating")

    def id: Rep[Long] = column[Long]("id", O.PrimaryKey, O.AutoInc)

    override def * : ProvenShape[Album] = (artistId, title, year, rating, id) <> (Album.tupled, Album.unapply)
  }
  lazy val albums = TableQuery[AlbumTable]

  // ==================================

  val db = Database.forConfig("h2mem")
  def run[T](action: DBIO[T]): T = Await.result(db.run(action), 3 seconds)
  val createTablesAction =
    artists.schema.create andThen
      albums.schema.create
  run(createTablesAction)

  // ==================================

  val seedingAction: DBIOAction[Unit, NoStream, Effect.Write] = for {
    celineDion    <- artists returning artists.map(_.id) into ((artist, id) => artist.copy(id = id)) += Artist("Celine Dion")
    _             <- artists += Artist("Toni Braxton")
    fairuzId      <- artists returning artists.map(_.id) += Artist("Fairuz")
    _             <- albums += Album(celineDion.id, "Let's talk about love", 1997, Rating.Good)
    _             <- albums += Album(celineDion.id, "A new day has come", 2001, Rating.Excellent)
    _             <- albums += Album(fairuzId, "Ana w sahrane", 2001, Rating.Good)
  } yield ()
  run(seedingAction)

  // ==================================

  val implicitJoinQuery: Query[(ArtistTable, AlbumTable), (Artist, Album), Seq] = for {
    artist <- artists
    album <- albums
    if artist.id === album.artistId
  } yield (artist, album)
  val implicitJoinAction = implicitJoinQuery
    .sortBy {
      case (artist, _) => artist.id.desc
    }
    .result
  println(implicitJoinAction.statements)
  run(implicitJoinAction).foreach(println)
  println("=============================")

  // ==================================

  val explicitJoinQuery = artists join albums on { (artist, album) => artist.id === album.artistId }
  val explicitJoinAction = explicitJoinQuery
    .sortBy {
      case (artist, _) => artist.id.desc
    }
    .result
  println(explicitJoinAction.statements)
  run(explicitJoinAction).foreach(println)
  println("=============================")

  // ==================================

  val explicitLeftOuterJoinQuery = artists joinLeft albums on (_.id === _.artistId)
  val explicitLeftOuterJoinAction = explicitLeftOuterJoinQuery
    .sortBy {
      case (artist, _) => artist.id.desc
    }
    .result
  println(explicitLeftOuterJoinAction.statements)
  run(explicitLeftOuterJoinAction).foreach(println)
  println("=============================")


}
