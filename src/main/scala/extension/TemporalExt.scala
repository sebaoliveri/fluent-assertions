package extension

import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}

import expression.{Bool, IsQuantifiableEqualToExp, LogicalOperatorsExp, QuantifiableExp}

class TemporalExt {


  implicit class InstantExtensions(instant: Instant) {

    import expression.DateExp._

    def isAfter(anotherInstant: Instant): LogicalOperatorsExp[Unit,Bool] =
      instantConstant(instant).isGreaterThan(QuantifiableExp(_ => anotherInstant))

    def isBefore(anotherInstant: Instant): LogicalOperatorsExp[Unit,Bool] =
      instantConstant(instant).isLessThan(QuantifiableExp(_ => anotherInstant))

    def isAfterOrSameThan(anotherInstant: Instant): LogicalOperatorsExp[Unit,Bool] =
      instantConstant(instant).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherInstant))

    def isBeforeOrSameThan(anotherInstant: Instant): LogicalOperatorsExp[Unit,Bool] =
      instantConstant(instant).isLessThanOrEqualTo(QuantifiableExp(_ => anotherInstant))

    def isBetween(min: Instant, max: Instant): LogicalOperatorsExp[Unit,Bool] =
      isAfterOrSameThan(min).and(isBeforeOrSameThan(max))

    def isEqualTo(anotherInstant: Instant): IsQuantifiableEqualToExp[Unit,Instant] =
      instantConstant(instant).isEqualTo(QuantifiableExp(_ => anotherInstant))
  }

  implicit class ZonedDateTimeExtensions(zonedDateTime: ZonedDateTime) {

    import expression.DateExp._

    def isAfter(anotherZonedDateTime: ZonedDateTime): LogicalOperatorsExp[Unit,Bool] =
      zonedDateTimeConstant(zonedDateTime).isGreaterThan(QuantifiableExp(_ => anotherZonedDateTime))

    def isBefore(anotherZonedDateTime: ZonedDateTime): LogicalOperatorsExp[Unit,Bool] =
      zonedDateTimeConstant(zonedDateTime).isLessThan(QuantifiableExp(_ => anotherZonedDateTime))

    def isAfterOrSameThan(anotherZonedDateTime: ZonedDateTime): LogicalOperatorsExp[Unit,Bool] =
      zonedDateTimeConstant(zonedDateTime).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherZonedDateTime))

    def isBeforeOrSameThan(anotherZonedDateTime: ZonedDateTime): LogicalOperatorsExp[Unit,Bool] =
      zonedDateTimeConstant(zonedDateTime).isLessThanOrEqualTo(QuantifiableExp(_ => anotherZonedDateTime))

    def isBetween(min: ZonedDateTime, max: ZonedDateTime): LogicalOperatorsExp[Unit,Bool] =
      isAfterOrSameThan(min).and(isBeforeOrSameThan(max))

    def isEqualTo(anotherZonedDateTime: ZonedDateTime): IsQuantifiableEqualToExp[Unit,ZonedDateTime] =
      zonedDateTimeConstant(zonedDateTime).isEqualTo(QuantifiableExp(_ => anotherZonedDateTime))
  }

  implicit class LocalDateExtensions(localDate: LocalDate) {

    import expression.DateExp._

    def isAfter(anotherLocalDate: LocalDate): LogicalOperatorsExp[Unit,Bool] =
      localDateConstant(localDate).isGreaterThan(QuantifiableExp(_ => anotherLocalDate))

    def isBefore(anotherLocalDate: LocalDate): LogicalOperatorsExp[Unit,Bool] =
      localDateConstant(localDate).isLessThan(QuantifiableExp(_ => anotherLocalDate))

    def isAfterOrSameThan(anotherLocalDate: LocalDate): LogicalOperatorsExp[Unit,Bool] =
      localDateConstant(localDate).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherLocalDate))

    def isBeforeOrSameThan(anotherLocalDate: LocalDate): LogicalOperatorsExp[Unit,Bool] =
      localDateConstant(localDate).isLessThanOrEqualTo(QuantifiableExp(_ => anotherLocalDate))

    def isBetween(min: LocalDate, max: LocalDate): LogicalOperatorsExp[Unit,Bool] =
      isAfterOrSameThan(min).and(isBeforeOrSameThan(max))

    def isEqualTo(anotherLocalDate: LocalDate): IsQuantifiableEqualToExp[Unit,LocalDate] =
      localDateConstant(localDate).isEqualTo(QuantifiableExp(_ => anotherLocalDate))
  }

  implicit class LocalDateTimeExtensions(localDateTime: LocalDateTime) {

    import expression.DateExp._

    def isAfter(anotherLocalDateTime: LocalDateTime): LogicalOperatorsExp[Unit,Bool] =
      localDateTimeConstant(localDateTime).isGreaterThan(QuantifiableExp(_ => anotherLocalDateTime))

    def isBefore(anotherLocalDateTime: LocalDateTime): LogicalOperatorsExp[Unit,Bool] =
      localDateTimeConstant(localDateTime).isLessThan(QuantifiableExp(_ => anotherLocalDateTime))

    def isAfterOrSameThan(anotherLocalDateTime: LocalDateTime): LogicalOperatorsExp[Unit,Bool] =
      localDateTimeConstant(localDateTime).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherLocalDateTime))

    def isBeforeOrSameThan(anotherLocalDateTime: LocalDateTime): LogicalOperatorsExp[Unit,Bool] =
      localDateTimeConstant(localDateTime).isLessThanOrEqualTo(QuantifiableExp(_ => anotherLocalDateTime))

    def isBetween(min: LocalDateTime, max: LocalDateTime): LogicalOperatorsExp[Unit,Bool] =
      isAfterOrSameThan(min).and(isBeforeOrSameThan(max))

    def isEqualTo(anotherLocalDateTime: LocalDateTime): IsQuantifiableEqualToExp[Unit,LocalDateTime] =
      localDateTimeConstant(localDateTime).isEqualTo(QuantifiableExp(_ => anotherLocalDateTime))
  }
}
