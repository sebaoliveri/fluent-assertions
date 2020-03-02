package org.nulluncertainty.expression

import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}

object TemporalExp {

  def instantConstant[T](instant: Instant): QuantifiableOrderedExp[T,Instant] =
    instantVariable(_ => instant)

  def instantVariable[T](f: T => Instant): QuantifiableOrderedExp[T,Instant] =
    QuantifiableOrderedExp(f.andThen(OrderedInstant))

  def zonedDateTimeConstant[T](dateTime: => ZonedDateTime): QuantifiableOrderedExp[T,ZonedDateTime] =
    zonedDateTimeVariable(_ => dateTime)

  def zonedDateTimeVariable[T](f: T => ZonedDateTime): QuantifiableOrderedExp[T,ZonedDateTime] =
    QuantifiableOrderedExp(f.andThen(OrderedZonedDateTime))

  def localDateConstant[T](date: => LocalDate): QuantifiableOrderedExp[T,LocalDate] =
    localDateVariable(_ => date)

  def localDateVariable[T](f: T => LocalDate): QuantifiableOrderedExp[T,LocalDate] =
    QuantifiableOrderedExp(f.andThen(OrderedLocalDate))

  def localDateTimeConstant[T](dateTime: => LocalDateTime): QuantifiableOrderedExp[T,LocalDateTime] =
    localDateTimeVariable(_ => dateTime)

  def localDateTimeVariable[T](f: T => LocalDateTime): QuantifiableOrderedExp[T,LocalDateTime] =
    QuantifiableOrderedExp(f.andThen(OrderedLocalDateTime))

  def maybeInstantConstant[T](maybeInstant: Option[Instant]): OptionalExp[T, Ordered[Instant]] =
    maybeInstantVariable(_ => maybeInstant)

  def maybeInstantVariable[T](maybeInstant: T => Option[Instant]): OptionalExp[T, Ordered[Instant]] =
    OptionalExp(maybeInstant.andThen(_.map(OrderedInstant)))

  def maybeZonedDateTimeConstant[T](maybeZonedDateTimeConstant: Option[ZonedDateTime]): OptionalExp[T, Ordered[ZonedDateTime]] =
    maybeZonedDateTimeVariable(_ => maybeZonedDateTimeConstant)

  def maybeZonedDateTimeVariable[T](maybeZonedDateTimeConstant: T => Option[ZonedDateTime]): OptionalExp[T, Ordered[ZonedDateTime]] =
    OptionalExp(maybeZonedDateTimeConstant.andThen(_.map(OrderedZonedDateTime)))

  def maybeLocalDateConstant[T](maybeLocalDate: Option[LocalDate]): OptionalExp[T, Ordered[LocalDate]] =
    maybeLocalDateVariable(_ => maybeLocalDate)

  def maybeLocalDateVariable[T](maybeLocalDate: T => Option[LocalDate]): OptionalExp[T, Ordered[LocalDate]] =
    OptionalExp(maybeLocalDate.andThen(_.map(OrderedLocalDate)))

  def maybeLocalDateTimeConstant[T](maybeLocalDateTime: Option[LocalDateTime]): OptionalExp[T, Ordered[LocalDateTime]] =
    maybeLocalDateTimeVariable(_ => maybeLocalDateTime)

  def maybeLocalDateTimeVariable[T](maybeLocalDateTime: T => Option[LocalDateTime]): OptionalExp[T, Ordered[LocalDateTime]] =
    OptionalExp(maybeLocalDateTime.andThen(_.map(OrderedLocalDateTime)))
}

case class OrderedInstant(value: Instant) extends Ordered[Instant] {

  override def compare(that: Instant): Int = value.compareTo(that)
}

case class OrderedZonedDateTime(value: ZonedDateTime) extends Ordered[ZonedDateTime] {

  override def compare(that: ZonedDateTime): Int = value.compareTo(that)
}

case class OrderedLocalDateTime(value: LocalDateTime) extends Ordered[LocalDateTime] {

  override def compare(that: LocalDateTime): Int = value.compareTo(that)
}

case class OrderedLocalDate(value: LocalDate) extends Ordered[LocalDate] {

  override def compare(that: LocalDate): Int = value.compareTo(that)
}
