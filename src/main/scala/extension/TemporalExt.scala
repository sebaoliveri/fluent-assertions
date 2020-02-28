package extension

import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}
import expression.{BoolExpBehaviour, IsQuantifiableEqualToExp, QuantifiableExp}

object TemporalExt {

  implicit class InstantExtensions(instant: Instant) {

    import expression.TemporalExp._

    def isAfterExp(anotherInstant: Instant): BoolExpBehaviour[Unit] =
      instantConstant(instant).isGreaterThan(QuantifiableExp(_ => anotherInstant))

    def isBeforeExp(anotherInstant: Instant): BoolExpBehaviour[Unit] =
      instantConstant(instant).isLessThan(QuantifiableExp(_ => anotherInstant))

    def isAfterOrSameThanExp(anotherInstant: Instant): BoolExpBehaviour[Unit] =
      instantConstant(instant).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherInstant))

    def isBeforeOrSameThanExp(anotherInstant: Instant): BoolExpBehaviour[Unit] =
      instantConstant(instant).isLessThanOrEqualTo(QuantifiableExp(_ => anotherInstant))

    def isInBetweenExp(min: Instant, max: Instant): BoolExpBehaviour[Unit] =
      isAfterOrSameThanExp(min).and(isBeforeOrSameThanExp(max))

    def isEqualToExp(anotherInstant: Instant): IsQuantifiableEqualToExp[Unit,Instant] =
      instantConstant(instant).isEqualTo(QuantifiableExp(_ => anotherInstant))
  }

  implicit class ZonedDateTimeExtensions(zonedDateTime: ZonedDateTime) {

    import expression.TemporalExp._

    def isAfterExp(anotherZonedDateTime: ZonedDateTime): BoolExpBehaviour[Unit] =
      zonedDateTimeConstant(zonedDateTime).isGreaterThan(QuantifiableExp(_ => anotherZonedDateTime))

    def isBeforeExp(anotherZonedDateTime: ZonedDateTime): BoolExpBehaviour[Unit] =
      zonedDateTimeConstant(zonedDateTime).isLessThan(QuantifiableExp(_ => anotherZonedDateTime))

    def isAfterOrSameThanExp(anotherZonedDateTime: ZonedDateTime): BoolExpBehaviour[Unit] =
      zonedDateTimeConstant(zonedDateTime).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherZonedDateTime))

    def isBeforeOrSameThanExp(anotherZonedDateTime: ZonedDateTime): BoolExpBehaviour[Unit] =
      zonedDateTimeConstant(zonedDateTime).isLessThanOrEqualTo(QuantifiableExp(_ => anotherZonedDateTime))

    def isInBetweenExp(min: ZonedDateTime, max: ZonedDateTime): BoolExpBehaviour[Unit] =
      isAfterOrSameThanExp(min).and(isBeforeOrSameThanExp(max))

    def isEqualToExp(anotherZonedDateTime: ZonedDateTime): IsQuantifiableEqualToExp[Unit,ZonedDateTime] =
      zonedDateTimeConstant(zonedDateTime).isEqualTo(QuantifiableExp(_ => anotherZonedDateTime))
  }

  implicit class LocalDateExtensions(localDate: LocalDate) {

    import expression.TemporalExp._

    def isAfterExp(anotherLocalDate: LocalDate): BoolExpBehaviour[Unit] =
      localDateConstant(localDate).isGreaterThan(QuantifiableExp(_ => anotherLocalDate))

    def isBeforeExp(anotherLocalDate: LocalDate): BoolExpBehaviour[Unit] =
      localDateConstant(localDate).isLessThan(QuantifiableExp(_ => anotherLocalDate))

    def isAfterOrSameThanExp(anotherLocalDate: LocalDate): BoolExpBehaviour[Unit] =
      localDateConstant(localDate).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherLocalDate))

    def isBeforeOrSameThanExp(anotherLocalDate: LocalDate): BoolExpBehaviour[Unit] =
      localDateConstant(localDate).isLessThanOrEqualTo(QuantifiableExp(_ => anotherLocalDate))

    def isInBetweenExp(min: LocalDate, max: LocalDate): BoolExpBehaviour[Unit] =
      isAfterOrSameThanExp(min).and(isBeforeOrSameThanExp(max))

    def isEqualToExp(anotherLocalDate: LocalDate): IsQuantifiableEqualToExp[Unit,LocalDate] =
      localDateConstant(localDate).isEqualTo(QuantifiableExp(_ => anotherLocalDate))
  }

  implicit class LocalDateTimeExtensions(localDateTime: LocalDateTime) {

    import expression.TemporalExp._

    def isAfterExp(anotherLocalDateTime: LocalDateTime): BoolExpBehaviour[Unit] =
      localDateTimeConstant(localDateTime).isGreaterThan(QuantifiableExp(_ => anotherLocalDateTime))

    def isBeforeExp(anotherLocalDateTime: LocalDateTime): BoolExpBehaviour[Unit] =
      localDateTimeConstant(localDateTime).isLessThan(QuantifiableExp(_ => anotherLocalDateTime))

    def isAfterOrSameThanExp(anotherLocalDateTime: LocalDateTime): BoolExpBehaviour[Unit] =
      localDateTimeConstant(localDateTime).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherLocalDateTime))

    def isBeforeOrSameThanExp(anotherLocalDateTime: LocalDateTime): BoolExpBehaviour[Unit] =
      localDateTimeConstant(localDateTime).isLessThanOrEqualTo(QuantifiableExp(_ => anotherLocalDateTime))

    def isInBetweenExp(min: LocalDateTime, max: LocalDateTime): BoolExpBehaviour[Unit] =
      isAfterOrSameThanExp(min).and(isBeforeOrSameThanExp(max))

    def isEqualToExp(anotherLocalDateTime: LocalDateTime): IsQuantifiableEqualToExp[Unit,LocalDateTime] =
      localDateTimeConstant(localDateTime).isEqualTo(QuantifiableExp(_ => anotherLocalDateTime))
  }
}
