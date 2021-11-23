package com.slick.learning

import slick.jdbc.H2Profile.{BaseColumnType, MappedColumnType}

import slick.driver.H2Driver.api._

sealed abstract class Rating(val value: Int)
object Rating {
  final case object Terrible extends Rating(1)
  final case object Bad extends Rating(2)
  final case object Fair extends Rating(3)
  final case object Good extends Rating(4)
  final case object Excellent extends Rating(5)

  val values: Array[Rating] = Array(Terrible, Bad, Fair, Good, Excellent)

  implicit val columnType: BaseColumnType[Rating] =
    MappedColumnType.base[Rating, Int](_.value, value => Rating.values(value - 1))
}
