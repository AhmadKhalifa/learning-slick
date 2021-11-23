package com.slick.learning

import com.slick.learning.Main.Album
import slick.dbio.Effect
import slick.driver.H2Driver.api._
import slick.lifted.ProvenShape
import slick.sql.{FixedSqlAction, FixedSqlStreamingAction}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.util.Success

object Main extends App {


  case class Album(artist: String, title: String, year: Int, rating: Rating, id: Long = 0L)

  class AlbumTable(tag: Tag) extends Table[Album](tag, "albums") {
    def artist: Rep[String] = column[String]("artist")

    def title: Rep[String] = column[String]("title")

    def year: Rep[Int] = column[Int]("year")

    def rating: Rep[Rating] = column[Rating]("rating")

    def id: Rep[Long] = column[Long]("id", O.PrimaryKey, O.AutoInc)

    override def * : ProvenShape[Album] = (artist, title, year, rating, id) <> (Album.tupled, Album.unapply)
  }

  lazy val albumTable = TableQuery[AlbumTable]

  val createAlbumTableAction = albumTable.schema.create
  val insertAlbumsAction: FixedSqlAction[Option[Int], NoStream, Effect.Write] = albumTable ++= Seq(
    Album("A1", "aaa", 2002, Rating.Good),
    Album("A2", "bbb", 2001, Rating.Bad),
    Album("A3", "ccc", 2003, Rating.Excellent)
  )
  val selectAllAlbumsAction: FixedSqlStreamingAction[Seq[Album], Album, Effect.Read] = albumTable
    .result

  val customQuery = albumTable
    .filter(album => album.year > 1990 && album.rating >= (Rating.Fair : Rating))
    .sortBy(_.artist.asc)
    .result

  val updateQueryAction = albumTable
    .filter(_.artist === "A1")
    .map(_.rating)
    .update(Rating.Terrible)

  val insertOneQueryAction: FixedSqlAction[Int, NoStream, Effect.Write] =
    albumTable += Album("A4", "ddd", 1999, Rating.Fair)

  val deleteQueryAction: FixedSqlAction[Int, NoStream, Effect.Write] = albumTable
    .filter(_.rating === (Rating.Terrible: Rating))
    .delete

  def disModernAlbumsAction(year: Int) = albumTable
    .filter(_.year > year)
    .map(_.rating)
    .update(Rating.Bad)
    .filter(_ > 0)

  val db = Database.forConfig("h2mem")

  def insertNewAlbumWithFutures(artist: String, title: String, year: Int): Future[Album] = {

    def alreadyHaveAlbums(): Future[Boolean] = db
      .run(
        albumTable
          .filter(_.artist === artist)
          .result
      )
      .map(_.nonEmpty)

    def insertWithRating(rating: Rating): Future[Album] = db
      .run(
        (albumTable returning albumTable.map(_.id) into ((album, id) => album.copy(id = id)))
          += Album(artist, title, year, rating)
      )

    def getRating(alreadyHaveAlbums: Boolean) = if (alreadyHaveAlbums) Rating.Good else Rating.Bad

    for {
      alreadyHaveAlbums <- alreadyHaveAlbums()
      insertedAlbum <- insertWithRating(getRating(alreadyHaveAlbums))
    } yield insertedAlbum
  }

  def insertNewAlbumWithActions(artist: String, title: String, year: Int): DBIOAction[Album, NoStream, Effect.Read with Effect.Write] = {

    def alreadyHaveAlbums(): DBIOAction[Boolean, NoStream, Effect.Read] = albumTable
      .filter(_.artist === artist)
      .result
      .map(_.nonEmpty)

    def insertWithRating(rating: Rating): FixedSqlAction[Album, NoStream, Effect.Write] =
      (albumTable returning albumTable.map(_.id) into ((album, id) => album.copy(id = id)))+= Album(artist, title, year, rating)

    def getRating(alreadyHaveAlbums: Boolean): Rating = if (alreadyHaveAlbums) Rating.Bad else Rating.Good

    for {
      alreadyHaveAlbums <- alreadyHaveAlbums()
      insertedAlbum <- insertWithRating(getRating(alreadyHaveAlbums))
    } yield insertedAlbum
  }

  val records = for {
    _ <- db.run(createAlbumTableAction)
    _ <- db.run(insertAlbumsAction)
    result <- db.run(customQuery)
  } yield result

  println(Await.result(records, 2 seconds))

  println("-========================")
  println(Await.result(db.run(insertNewAlbumWithActions("A1", "zzz", 2013).transactionally), 3 seconds))
  println(Await.result(db.run(insertNewAlbumWithActions("A9", "ccc", 2013)), 3 seconds))
  println(Await.result(db.run(insertNewAlbumWithActions("A9", "ggg", 2013)), 3 seconds))

  println(Await.result(insertNewAlbumWithFutures("A2", "zzz", 2013), 3 seconds))
  println(Await.result(insertNewAlbumWithFutures("A10", "zzz", 2013), 3 seconds))
  println(Await.result(insertNewAlbumWithFutures("A10", "hhh", 2013), 3 seconds))

}
