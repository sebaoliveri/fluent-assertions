package org.nulluncertainty.extension

import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}
import org.nulluncertainty.expression.{ComposableBooleanExp, IsQuantifiableEqualToExp, QuantifiableExp}

object TemporalExt {

  implicit class InstantExtensions(instant: Instant) {

    import org.nulluncertainty.expression.TemporalExp._

    def isAfterExp(anotherInstant: Instant): ComposableBooleanExp[Unit] =
      instantConstant(instant).isGreaterThan(QuantifiableExp(_ => anotherInstant))

    def isBeforeExp(anotherInstant: Instant): ComposableBooleanExp[Unit] =
      instantConstant(instant).isLessThan(QuantifiableExp(_ => anotherInstant))

    def isAfterOrSameThanExp(anotherInstant: Instant): ComposableBooleanExp[Unit] =
      instantConstant(instant).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherInstant))

    def isBeforeOrSameThanExp(anotherInstant: Instant): ComposableBooleanExp[Unit] =
      instantConstant(instant).isLessThanOrEqualTo(QuantifiableExp(_ => anotherInstant))

    def isInBetweenExp(min: Instant, max: Instant): ComposableBooleanExp[Unit] =
      isAfterOrSameThanExp(min).and(isBeforeOrSameThanExp(max))

    def isEqualToExp(anotherInstant: Instant): IsQuantifiableEqualToExp[Unit,Instant] =
      instantConstant(instant).isEqualTo(QuantifiableExp(_ => anotherInstant))
  }

  implicit class ZonedDateTimeExtensions(zonedDateTime: ZonedDateTime) {

    import org.nulluncertainty.expression.TemporalExp._

    def isAfterExp(anotherZonedDateTime: ZonedDateTime): ComposableBooleanExp[Unit] =
      zonedDateTimeConstant(zonedDateTime).isGreaterThan(QuantifiableExp(_ => anotherZonedDateTime))

    def isBeforeExp(anotherZonedDateTime: ZonedDateTime): ComposableBooleanExp[Unit] =
      zonedDateTimeConstant(zonedDateTime).isLessThan(QuantifiableExp(_ => anotherZonedDateTime))

    def isAfterOrSameThanExp(anotherZonedDateTime: ZonedDateTime): ComposableBooleanExp[Unit] =
      zonedDateTimeConstant(zonedDateTime).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherZonedDateTime))

    def isBeforeOrSameThanExp(anotherZonedDateTime: ZonedDateTime): ComposableBooleanExp[Unit] =
      zonedDateTimeConstant(zonedDateTime).isLessThanOrEqualTo(QuantifiableExp(_ => anotherZonedDateTime))

    def isInBetweenExp(min: ZonedDateTime, max: ZonedDateTime): ComposableBooleanExp[Unit] =
      isAfterOrSameThanExp(min).and(isBeforeOrSameThanExp(max))

    def isEqualToExp(anotherZonedDateTime: ZonedDateTime): IsQuantifiableEqualToExp[Unit,ZonedDateTime] =
      zonedDateTimeConstant(zonedDateTime).isEqualTo(QuantifiableExp(_ => anotherZonedDateTime))
  }

  implicit class LocalDateExtensions(localDate: LocalDate) {

    import org.nulluncertainty.expression.TemporalExp._

    def isAfterExp(anotherLocalDate: LocalDate): ComposableBooleanExp[Unit] =
      localDateConstant(localDate).isGreaterThan(QuantifiableExp(_ => anotherLocalDate))

    def isBeforeExp(anotherLocalDate: LocalDate): ComposableBooleanExp[Unit] =
      localDateConstant(localDate).isLessThan(QuantifiableExp(_ => anotherLocalDate))

    def isAfterOrSameThanExp(anotherLocalDate: LocalDate): ComposableBooleanExp[Unit] =
      localDateConstant(localDate).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherLocalDate))

    def isBeforeOrSameThanExp(anotherLocalDate: LocalDate): ComposableBooleanExp[Unit] =
      localDateConstant(localDate).isLessThanOrEqualTo(QuantifiableExp(_ => anotherLocalDate))

    def isInBetweenExp(min: LocalDate, max: LocalDate): ComposableBooleanExp[Unit] =
      isAfterOrSameThanExp(min).and(isBeforeOrSameThanExp(max))

    def isEqualToExp(anotherLocalDate: LocalDate): IsQuantifiableEqualToExp[Unit,LocalDate] =
      localDateConstant(localDate).isEqualTo(QuantifiableExp(_ => anotherLocalDate))
  }

  implicit class LocalDateTimeExtensions(localDateTime: LocalDateTime) {

    import org.nulluncertainty.expression.TemporalExp._

    def isAfterExp(anotherLocalDateTime: LocalDateTime): ComposableBooleanExp[Unit] =
      localDateTimeConstant(localDateTime).isGreaterThan(QuantifiableExp(_ => anotherLocalDateTime))

    def isBeforeExp(anotherLocalDateTime: LocalDateTime): ComposableBooleanExp[Unit] =
      localDateTimeConstant(localDateTime).isLessThan(QuantifiableExp(_ => anotherLocalDateTime))

    def isAfterOrSameThanExp(anotherLocalDateTime: LocalDateTime): ComposableBooleanExp[Unit] =
      localDateTimeConstant(localDateTime).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherLocalDateTime))

    def isBeforeOrSameThanExp(anotherLocalDateTime: LocalDateTime): ComposableBooleanExp[Unit] =
      localDateTimeConstant(localDateTime).isLessThanOrEqualTo(QuantifiableExp(_ => anotherLocalDateTime))

    def isInBetweenExp(min: LocalDateTime, max: LocalDateTime): ComposableBooleanExp[Unit] =
      isAfterOrSameThanExp(min).and(isBeforeOrSameThanExp(max))

    def isEqualToExp(anotherLocalDateTime: LocalDateTime): IsQuantifiableEqualToExp[Unit,LocalDateTime] =
      localDateTimeConstant(localDateTime).isEqualTo(QuantifiableExp(_ => anotherLocalDateTime))
  }
}
