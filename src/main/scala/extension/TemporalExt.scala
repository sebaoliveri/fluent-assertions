package extension

import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}

import expression.{Bool, IsQuantifiableEqualToExp, LogicalOperatorsExp, QuantifiableExp}

object TemporalExt {

  implicit class InstantExtensions(instant: Instant) {

    import expression.DateExp._

    def isAfter(anotherInstant: Instant): LogicalOperatorsExp[Unit,Bool] =
      instantConstant(instant).isGreaterThan(QuantifiableExp(_ => anotherInstant))

    def isBefore(anotherInstant: Instant): LogicalOperatorsExp[Unit,Bool] =
      instantConstant(instant).isLessThan(QuantifiableExp(_ => anotherInstant))

    def occursAfterOrIsSameThan(anotherInstant: Instant): LogicalOperatorsExp[Unit,Bool] =
      instantConstant(instant).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherInstant))

    def occursBeforeOrIsSameThan(anotherInstant: Instant): LogicalOperatorsExp[Unit,Bool] =
      instantConstant(instant).isLessThanOrEqualTo(QuantifiableExp(_ => anotherInstant))

    def isInBetween(min: Instant, max: Instant): LogicalOperatorsExp[Unit,Bool] =
      occursAfterOrIsSameThan(min).and(occursBeforeOrIsSameThan(max))

    def equalsTo(anotherInstant: Instant): IsQuantifiableEqualToExp[Unit,Instant] =
      instantConstant(instant).isEqualTo(QuantifiableExp(_ => anotherInstant))
  }

  implicit class ZonedDateTimeExtensions(zonedDateTime: ZonedDateTime) {

    import expression.DateExp._

    def isAfter(anotherZonedDateTime: ZonedDateTime): LogicalOperatorsExp[Unit,Bool] =
      zonedDateTimeConstant(zonedDateTime).isGreaterThan(QuantifiableExp(_ => anotherZonedDateTime))

    def isBefore(anotherZonedDateTime: ZonedDateTime): LogicalOperatorsExp[Unit,Bool] =
      zonedDateTimeConstant(zonedDateTime).isLessThan(QuantifiableExp(_ => anotherZonedDateTime))

    def occursAfterOrIsSameThan(anotherZonedDateTime: ZonedDateTime): LogicalOperatorsExp[Unit,Bool] =
      zonedDateTimeConstant(zonedDateTime).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherZonedDateTime))

    def occursBeforeOrIsSameThan(anotherZonedDateTime: ZonedDateTime): LogicalOperatorsExp[Unit,Bool] =
      zonedDateTimeConstant(zonedDateTime).isLessThanOrEqualTo(QuantifiableExp(_ => anotherZonedDateTime))

    def isInBetween(min: ZonedDateTime, max: ZonedDateTime): LogicalOperatorsExp[Unit,Bool] =
      occursAfterOrIsSameThan(min).and(occursBeforeOrIsSameThan(max))

    def equalsTo(anotherZonedDateTime: ZonedDateTime): IsQuantifiableEqualToExp[Unit,ZonedDateTime] =
      zonedDateTimeConstant(zonedDateTime).isEqualTo(QuantifiableExp(_ => anotherZonedDateTime))
  }

  implicit class LocalDateExtensions(localDate: LocalDate) {

    import expression.DateExp._

    def isAfter(anotherLocalDate: LocalDate): LogicalOperatorsExp[Unit,Bool] =
      localDateConstant(localDate).isGreaterThan(QuantifiableExp(_ => anotherLocalDate))

    def isBefore(anotherLocalDate: LocalDate): LogicalOperatorsExp[Unit,Bool] =
      localDateConstant(localDate).isLessThan(QuantifiableExp(_ => anotherLocalDate))

    def occursAfterOrIsSameThan(anotherLocalDate: LocalDate): LogicalOperatorsExp[Unit,Bool] =
      localDateConstant(localDate).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherLocalDate))

    def occursBeforeOrIsSameThan(anotherLocalDate: LocalDate): LogicalOperatorsExp[Unit,Bool] =
      localDateConstant(localDate).isLessThanOrEqualTo(QuantifiableExp(_ => anotherLocalDate))

    def isInBetween(min: LocalDate, max: LocalDate): LogicalOperatorsExp[Unit,Bool] =
      occursAfterOrIsSameThan(min).and(occursBeforeOrIsSameThan(max))

    def equalsTo(anotherLocalDate: LocalDate): IsQuantifiableEqualToExp[Unit,LocalDate] =
      localDateConstant(localDate).isEqualTo(QuantifiableExp(_ => anotherLocalDate))
  }

  implicit class LocalDateTimeExtensions(localDateTime: LocalDateTime) {

    import expression.DateExp._

    def occursAfter(anotherLocalDateTime: LocalDateTime): LogicalOperatorsExp[Unit,Bool] =
      localDateTimeConstant(localDateTime).isGreaterThan(QuantifiableExp(_ => anotherLocalDateTime))

    def occursBefore(anotherLocalDateTime: LocalDateTime): LogicalOperatorsExp[Unit,Bool] =
      localDateTimeConstant(localDateTime).isLessThan(QuantifiableExp(_ => anotherLocalDateTime))

    def occursAfterOrIsSameThan(anotherLocalDateTime: LocalDateTime): LogicalOperatorsExp[Unit,Bool] =
      localDateTimeConstant(localDateTime).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherLocalDateTime))

    def occursBeforeOrIsSameThan(anotherLocalDateTime: LocalDateTime): LogicalOperatorsExp[Unit,Bool] =
      localDateTimeConstant(localDateTime).isLessThanOrEqualTo(QuantifiableExp(_ => anotherLocalDateTime))

    def isInBetween(min: LocalDateTime, max: LocalDateTime): LogicalOperatorsExp[Unit,Bool] =
      occursAfterOrIsSameThan(min).and(occursBeforeOrIsSameThan(max))

    def equalsTo(anotherLocalDateTime: LocalDateTime): IsQuantifiableEqualToExp[Unit,LocalDateTime] =
      localDateTimeConstant(localDateTime).isEqualTo(QuantifiableExp(_ => anotherLocalDateTime))
  }
}
