package assertion

import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}
import expression._

object DateExpAssertionBuilder {
  import DateExp._
  def fromInstantConstant[T](instant: Instant): DateExpAssertionBuilder[T,Instant] =
    DateExpAssertionBuilder(instantConstant(instant))
  def fromZonedDateTimeConstant[T](zonedDateTime: ZonedDateTime): DateExpAssertionBuilder[T,ZonedDateTime] =
    DateExpAssertionBuilder(zonedDateTimeConstant(zonedDateTime))
  def fromLocalDateConstant[T](localDate: LocalDate): DateExpAssertionBuilder[T,LocalDate] =
    DateExpAssertionBuilder(localDateConstant(localDate))
  def fromLocalDateTimeConstant[T](localDateTime: LocalDateTime): DateExpAssertionBuilder[T,LocalDateTime] =
    DateExpAssertionBuilder(localDateTimeConstant(localDateTime))
  def fromInstantVariable[T](instant: T => Instant): DateExpAssertionBuilder[T,Instant] =
    DateExpAssertionBuilder(instantVariable(instant))
  def fromZonedDateTimeVariable[T](zonedDateTime: T => ZonedDateTime): DateExpAssertionBuilder[T,ZonedDateTime] =
    DateExpAssertionBuilder(zonedDateTimeVariable(zonedDateTime))
  def fromLocalDateVariable[T](localDate: T => LocalDate): DateExpAssertionBuilder[T,LocalDate] =
    DateExpAssertionBuilder(localDateVariable(localDate))
  def fromLocalDateTimeVariable[T](localDateTime: T => LocalDateTime): DateExpAssertionBuilder[T,LocalDateTime] =
    DateExpAssertionBuilder(localDateTimeVariable(localDateTime))
  def apply[T,R](dateExp: QuantifiableOrderedExp[T,R]): DateExpAssertionBuilder[T,R] =
    new DateExpAssertionBuilder(dateExp, new NullBooleanExp[T,Bool](), _ and _)
}

case class DateExpAssertionBuilder[T,R](quantifiableExp: QuantifiableOrderedExp[T,R], expression: BooleanExp[T,Bool], operator: (BooleanExp[T,Bool], BooleanExp[T,Bool]) => BooleanExp[T,Bool]) extends AssertionBuilder[T] {
  def isAfter(date: R): BoolExpAssertionBuilder[T, DateExpAssertionBuilder[T, R]] = isAfter(_ => date)
  def isAfter(date: T => R): BoolExpAssertionBuilder[T, DateExpAssertionBuilder[T, R]] = newBoolExp(quantifiableExp.isGreaterThan(QuantifiableExp(date)))
  def isBefore(date: R): BoolExpAssertionBuilder[T, DateExpAssertionBuilder[T, R]] = isBefore(_ => date)
  def isBefore(date: T => R): BoolExpAssertionBuilder[T, DateExpAssertionBuilder[T, R]] = newBoolExp(quantifiableExp.isLessThan(QuantifiableExp(date)))
  def isAfterOrSameThan(date: R): BoolExpAssertionBuilder[T, DateExpAssertionBuilder[T, R]] = isAfterOrSameThan(_ => date)
  def isAfterOrSameThan(date: T => R): BoolExpAssertionBuilder[T, DateExpAssertionBuilder[T, R]] = newBoolExp(quantifiableExp.isGreaterThanOrEqualTo(QuantifiableExp(date)))
  def isBeforeOrSameThan(date: R): BoolExpAssertionBuilder[T, DateExpAssertionBuilder[T, R]] = isBeforeOrSameThan(_ => date)
  def isBeforeOrSameThan(date: T => R): BoolExpAssertionBuilder[T, DateExpAssertionBuilder[T, R]] = newBoolExp(quantifiableExp.isLessThanOrEqualTo(QuantifiableExp(date)))
  def isBetween(min: R, max: R): BoolExpAssertionBuilder[T, DateExpAssertionBuilder[T, R]] = isBetween(_ => min, _ => max)
  def isBetween(min: T => R, max: T => R): BoolExpAssertionBuilder[T, DateExpAssertionBuilder[T, R]] = newBoolExp(quantifiableExp.isGreaterThanOrEqualTo(QuantifiableExp(min)).and(quantifiableExp.isLessThanOrEqualTo(QuantifiableExp(max))))
  def isEqualTo(date: R): BoolExpAssertionBuilder[T, DateExpAssertionBuilder[T, R]] = isEqualTo(_ => date)
  def isEqualTo(date: T => R): BoolExpAssertionBuilder[T, DateExpAssertionBuilder[T, R]] = newBoolExp(quantifiableExp.isEqualTo(QuantifiableExp(date)))
  private def newBoolExp(anExpression: BooleanExp[T,Bool]): BoolExpAssertionBuilder[T, DateExpAssertionBuilder[T,R]] =
    BoolExpAssertionBuilder(operator(expression, anExpression), (newExpression,newOperator) => DateExpAssertionBuilder(quantifiableExp, newExpression, newOperator))
}