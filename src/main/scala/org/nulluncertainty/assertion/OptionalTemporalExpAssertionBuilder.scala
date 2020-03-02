package org.nulluncertainty.assertion

import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}
import org.nulluncertainty.expression._

object OptionalTemporalExpAssertionBuilder {
  import TemporalExp._

  def fromMaybeInstantConstant[T,R](maybeInstant: Option[Instant]): OptionalTemporalExpAssertionBuilder[T,Instant] =
    fromMaybeInstantVariable(_ => maybeInstant)

  def fromMaybeZonedDateTimeConstant[T,R](maybeZonedDateTime: Option[ZonedDateTime]): OptionalTemporalExpAssertionBuilder[T,ZonedDateTime] =
    fromMaybeZonedDateTimeVariable(_ => maybeZonedDateTime)

  def fromMaybeLocalDateConstant[T,R](maybeLocalDate: Option[LocalDate]): OptionalTemporalExpAssertionBuilder[T,LocalDate] =
    fromMaybeLocalDateVariable(_ => maybeLocalDate)

  def fromMaybeLocalDateTimeConstant[T,R](maybeLocalDateTime: Option[LocalDateTime]): OptionalTemporalExpAssertionBuilder[T,LocalDateTime] =
    fromMaybeLocalDateTimeVariable(_ => maybeLocalDateTime)

  def fromMaybeInstantVariable[T,R](maybeInstant: T => Option[Instant]): OptionalTemporalExpAssertionBuilder[T,Instant] =
    OptionalTemporalExpAssertionBuilder(maybeInstantVariable(maybeInstant))

  def fromMaybeZonedDateTimeVariable[T,R](maybeZonedDateTime: T => Option[ZonedDateTime]): OptionalTemporalExpAssertionBuilder[T,ZonedDateTime] =
    OptionalTemporalExpAssertionBuilder(maybeZonedDateTimeVariable(maybeZonedDateTime))

  def fromMaybeLocalDateVariable[T,R](maybeLocalDate: T => Option[LocalDate]): OptionalTemporalExpAssertionBuilder[T,LocalDate] =
    OptionalTemporalExpAssertionBuilder(maybeLocalDateVariable(maybeLocalDate))

  def fromMaybeLocalDateTimeVariable[T,R](maybeLocalDateTime: T => Option[LocalDateTime]): OptionalTemporalExpAssertionBuilder[T,LocalDateTime] =
    OptionalTemporalExpAssertionBuilder(maybeLocalDateTimeVariable(maybeLocalDateTime))

  def apply[T,R](optionalExp: OptionalExp[T,Ordered[R]]): OptionalTemporalExpAssertionBuilder[T,R] =
    OptionalTemporalExpAssertionBuilder(optionalExp, new NullExp[T,Bool]())

  def apply[T,R](optionalExp: OptionalExp[T,Ordered[R]], expression: ComposableBooleanExp[T]): OptionalTemporalExpAssertionBuilder[T,R] =
    new OptionalTemporalExpAssertionBuilder(optionalExp, new NullExp[T,Bool](), _ and _)
}

case class OptionalTemporalExpAssertionBuilder[T,R](optionExp: OptionalExp[T,Ordered[R]], expression: ComposableBooleanExp[T], operator: (ComposableBooleanExp[T], ComposableBooleanExp[T]) => ComposableBooleanExp[T])
  extends AssertionBuilder[T,OptionalTemporalExpAssertionBuilder[T,R]](expression) {

  def isDefined: OptionalTemporalExpAssertionBuilder[T,R] =
    OptionalTemporalExpAssertionBuilder(optionExp, operator.apply(expression, IsDefinedExp[T,Ordered[R]](optionExp)))

  def isAfter(date: R): OptionalTemporalExpAssertionBuilder[T,R] =
    isAfter(_ => date)

  def isAfter(date: T => R): OptionalTemporalExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isGreaterThan(QuantifiableExp(date)))

  def isBefore(date: R): OptionalTemporalExpAssertionBuilder[T,R] =
    isBefore(_ => date)

  def isBefore(date: T => R): OptionalTemporalExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isLessThan(QuantifiableExp(date)))

  def isAfterOrSameThan(date: R): OptionalTemporalExpAssertionBuilder[T,R] =
    isAfterOrSameThan(_ => date)

  def isAfterOrSameThan(date: T => R): OptionalTemporalExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isGreaterThanOrEqualTo(QuantifiableExp(date)))

  def isBeforeOrSameThan(date: R): OptionalTemporalExpAssertionBuilder[T,R] =
    isBeforeOrSameThan(_ => date)

  def isBeforeOrSameThan(date: T => R): OptionalTemporalExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isLessThanOrEqualTo(QuantifiableExp(date)))

  def isInBetween(min: R, max: R): OptionalTemporalExpAssertionBuilder[T,R] =
    isInBetween(_ => min, _ => max)

  def isInBetween(min: T => R, max: T => R): OptionalTemporalExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isInInclusiveRange(QuantifiableExp(min), QuantifiableExp(max)))

  def isEqualTo(date: R): OptionalTemporalExpAssertionBuilder[T, R] =
    isEqualTo(_ => date)

  def isEqualTo(date: T => R): OptionalTemporalExpAssertionBuilder[T, R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isEqualTo(QuantifiableExp(date)))

  override def or: OptionalTemporalExpAssertionBuilder[T, R] =
    OptionalTemporalExpAssertionBuilder(optionExp, expression, _ or _)

  private def newWith(newExpression: Ordered[R] => ComposableBooleanExp[T]) =
    OptionalTemporalExpAssertionBuilder(optionExp,
      operator.apply(expression, OptionalBoolExp[T,Ordered[R]](optionExp, newExpression)))
}
