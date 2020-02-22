package extension

import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}

import expression.{Bool, IsQuantifiableEqualToExp, LogicalOperatorsExp, QuantifiableExp}

object TemporalExt {

  implicit class InstantExtensions(instant: Instant) {

    import expression.DateExp._

    def isAfterExp(anotherInstant: Instant): LogicalOperatorsExp[Unit,Bool] =
      instantConstant(instant).isGreaterThan(QuantifiableExp(_ => anotherInstant))

    def isBeforeExp(anotherInstant: Instant): LogicalOperatorsExp[Unit,Bool] =
      instantConstant(instant).isLessThan(QuantifiableExp(_ => anotherInstant))

    def isAfterOrSameThanExp(anotherInstant: Instant): LogicalOperatorsExp[Unit,Bool] =
      instantConstant(instant).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherInstant))

    def isBeforeOrSameThanExp(anotherInstant: Instant): LogicalOperatorsExp[Unit,Bool] =
      instantConstant(instant).isLessThanOrEqualTo(QuantifiableExp(_ => anotherInstant))

    def isInBetweenExp(min: Instant, max: Instant): LogicalOperatorsExp[Unit,Bool] =
      isAfterOrSameThanExp(min).and(isBeforeOrSameThanExp(max))

    def isEqualToExp(anotherInstant: Instant): IsQuantifiableEqualToExp[Unit,Instant] =
      instantConstant(instant).isEqualTo(QuantifiableExp(_ => anotherInstant))
  }

  implicit class ZonedDateTimeExtensions(zonedDateTime: ZonedDateTime) {

    import expression.DateExp._

    def isAfterExp(anotherZonedDateTime: ZonedDateTime): LogicalOperatorsExp[Unit,Bool] =
      zonedDateTimeConstant(zonedDateTime).isGreaterThan(QuantifiableExp(_ => anotherZonedDateTime))

    def isBeforeExp(anotherZonedDateTime: ZonedDateTime): LogicalOperatorsExp[Unit,Bool] =
      zonedDateTimeConstant(zonedDateTime).isLessThan(QuantifiableExp(_ => anotherZonedDateTime))

    def isAfterOrSameThanExp(anotherZonedDateTime: ZonedDateTime): LogicalOperatorsExp[Unit,Bool] =
      zonedDateTimeConstant(zonedDateTime).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherZonedDateTime))

    def isBeforeOrSameThanExp(anotherZonedDateTime: ZonedDateTime): LogicalOperatorsExp[Unit,Bool] =
      zonedDateTimeConstant(zonedDateTime).isLessThanOrEqualTo(QuantifiableExp(_ => anotherZonedDateTime))

    def isInBetweenExp(min: ZonedDateTime, max: ZonedDateTime): LogicalOperatorsExp[Unit,Bool] =
      isAfterOrSameThanExp(min).and(isBeforeOrSameThanExp(max))

    def isEqualToExp(anotherZonedDateTime: ZonedDateTime): IsQuantifiableEqualToExp[Unit,ZonedDateTime] =
      zonedDateTimeConstant(zonedDateTime).isEqualTo(QuantifiableExp(_ => anotherZonedDateTime))
  }

  implicit class LocalDateExtensions(localDate: LocalDate) {

    import expression.DateExp._

    def isAfterExp(anotherLocalDate: LocalDate): LogicalOperatorsExp[Unit,Bool] =
      localDateConstant(localDate).isGreaterThan(QuantifiableExp(_ => anotherLocalDate))

    def isBeforeExp(anotherLocalDate: LocalDate): LogicalOperatorsExp[Unit,Bool] =
      localDateConstant(localDate).isLessThan(QuantifiableExp(_ => anotherLocalDate))

    def isAfterOrSameThanExp(anotherLocalDate: LocalDate): LogicalOperatorsExp[Unit,Bool] =
      localDateConstant(localDate).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherLocalDate))

    def isBeforeOrSameThanExp(anotherLocalDate: LocalDate): LogicalOperatorsExp[Unit,Bool] =
      localDateConstant(localDate).isLessThanOrEqualTo(QuantifiableExp(_ => anotherLocalDate))

    def isInBetweenExp(min: LocalDate, max: LocalDate): LogicalOperatorsExp[Unit,Bool] =
      isAfterOrSameThanExp(min).and(isBeforeOrSameThanExp(max))

    def isEqualToExp(anotherLocalDate: LocalDate): IsQuantifiableEqualToExp[Unit,LocalDate] =
      localDateConstant(localDate).isEqualTo(QuantifiableExp(_ => anotherLocalDate))
  }

  implicit class LocalDateTimeExtensions(localDateTime: LocalDateTime) {

    import expression.DateExp._

    def isAfterExp(anotherLocalDateTime: LocalDateTime): LogicalOperatorsExp[Unit,Bool] =
      localDateTimeConstant(localDateTime).isGreaterThan(QuantifiableExp(_ => anotherLocalDateTime))

    def isBeforeExp(anotherLocalDateTime: LocalDateTime): LogicalOperatorsExp[Unit,Bool] =
      localDateTimeConstant(localDateTime).isLessThan(QuantifiableExp(_ => anotherLocalDateTime))

    def isAfterOrSameThanExp(anotherLocalDateTime: LocalDateTime): LogicalOperatorsExp[Unit,Bool] =
      localDateTimeConstant(localDateTime).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherLocalDateTime))

    def isBeforeOrSameThanExp(anotherLocalDateTime: LocalDateTime): LogicalOperatorsExp[Unit,Bool] =
      localDateTimeConstant(localDateTime).isLessThanOrEqualTo(QuantifiableExp(_ => anotherLocalDateTime))

    def isInBetweenExp(min: LocalDateTime, max: LocalDateTime): LogicalOperatorsExp[Unit,Bool] =
      isAfterOrSameThanExp(min).and(isBeforeOrSameThanExp(max))

    def isEqualToExp(anotherLocalDateTime: LocalDateTime): IsQuantifiableEqualToExp[Unit,LocalDateTime] =
      localDateTimeConstant(localDateTime).isEqualTo(QuantifiableExp(_ => anotherLocalDateTime))
  }
}
