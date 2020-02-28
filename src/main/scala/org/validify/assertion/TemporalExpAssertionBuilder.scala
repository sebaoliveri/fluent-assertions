package org.validify.assertion

import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}
import org.validify.expression._

object TemporalExpAssertionBuilder {

  import TemporalExp._

  def fromInstantConstant[T](instant: Instant): TemporalExpAssertionBuilder[T,Instant] =
    TemporalExpAssertionBuilder(instantConstant(instant))

  def fromZonedDateTimeConstant[T](zonedDateTime: ZonedDateTime): TemporalExpAssertionBuilder[T,ZonedDateTime] =
    TemporalExpAssertionBuilder(zonedDateTimeConstant(zonedDateTime))

  def fromLocalDateConstant[T](localDate: LocalDate): TemporalExpAssertionBuilder[T,LocalDate] =
    TemporalExpAssertionBuilder(localDateConstant(localDate))

  def fromLocalDateTimeConstant[T](localDateTime: LocalDateTime): TemporalExpAssertionBuilder[T,LocalDateTime] =
    TemporalExpAssertionBuilder(localDateTimeConstant(localDateTime))

  def fromInstantVariable[T](instant: T => Instant): TemporalExpAssertionBuilder[T,Instant] =
    TemporalExpAssertionBuilder(instantVariable(instant))

  def fromZonedDateTimeVariable[T](zonedDateTime: T => ZonedDateTime): TemporalExpAssertionBuilder[T,ZonedDateTime] =
    TemporalExpAssertionBuilder(zonedDateTimeVariable(zonedDateTime))

  def fromLocalDateVariable[T](localDate: T => LocalDate): TemporalExpAssertionBuilder[T,LocalDate] =
    TemporalExpAssertionBuilder(localDateVariable(localDate))

  def fromLocalDateTimeVariable[T](localDateTime: T => LocalDateTime): TemporalExpAssertionBuilder[T,LocalDateTime] =
    TemporalExpAssertionBuilder(localDateTimeVariable(localDateTime))

  def apply[T,R](dateExp: QuantifiableOrderedExp[T,R]): TemporalExpAssertionBuilder[T,R] =
    TemporalExpAssertionBuilder(dateExp, new NullExp[T,Bool]())

  def apply[T,R](dateExp: QuantifiableOrderedExp[T,R], expression: BoolExpBehaviour[T]): TemporalExpAssertionBuilder[T,R] =
    new TemporalExpAssertionBuilder(dateExp, expression, _ and _)
}

case class TemporalExpAssertionBuilder[T,R](quantifiableExp: QuantifiableOrderedExp[T,R], expression: BoolExpBehaviour[T], operator: (BoolExpBehaviour[T], BoolExpBehaviour[T]) => BoolExpBehaviour[T])
  extends AssertionBuilder[T,TemporalExpAssertionBuilder[T,R]](expression) {

  def isAfter(date: R): TemporalExpAssertionBuilder[T,R] =
    isAfter(_ => date)

  def isAfter(date: T => R): TemporalExpAssertionBuilder[T,R] =
    newWith(quantifiableExp.isGreaterThan(QuantifiableExp(date)))

  def isBefore(date: R): TemporalExpAssertionBuilder[T,R] =
    isBefore(_ => date)

  def isBefore(date: T => R): TemporalExpAssertionBuilder[T,R] =
    newWith(quantifiableExp.isLessThan(QuantifiableExp(date)))

  def isAfterOrSameThan(date: R): TemporalExpAssertionBuilder[T,R] =
    isAfterOrSameThan(_ => date)

  def isAfterOrSameThan(date: T => R): TemporalExpAssertionBuilder[T,R] =
    newWith(quantifiableExp.isGreaterThanOrEqualTo(QuantifiableExp(date)))

  def isBeforeOrSameThan(date: R): TemporalExpAssertionBuilder[T,R] =
    isBeforeOrSameThan(_ => date)

  def isBeforeOrSameThan(date: T => R): TemporalExpAssertionBuilder[T,R] =
    newWith(quantifiableExp.isLessThanOrEqualTo(QuantifiableExp(date)))

  def isInBetween(min: R, max: R): TemporalExpAssertionBuilder[T,R] =
    isInBetween(_ => min, _ => max)

  def isInBetween(min: T => R, max: T => R): TemporalExpAssertionBuilder[T,R] =
    newWith(quantifiableExp.isGreaterThanOrEqualTo(QuantifiableExp(min)).and(quantifiableExp.isLessThanOrEqualTo(QuantifiableExp(max))))

  def isEqualTo(date: R): TemporalExpAssertionBuilder[T,R] =
    isEqualTo(_ => date)

  def isEqualTo(date: T => R): TemporalExpAssertionBuilder[T,R] =
    newWith(quantifiableExp.isEqualTo(QuantifiableExp(date)))

  override def or: TemporalExpAssertionBuilder[T, R] =
    TemporalExpAssertionBuilder(quantifiableExp, expression, _ or _)

  private def newWith(newExpression: BoolExpBehaviour[T]): TemporalExpAssertionBuilder[T, R] =
    TemporalExpAssertionBuilder(quantifiableExp, operator.apply(expression, newExpression))
}
