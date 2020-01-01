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
    DateExpAssertionBuilder(dateExp, new NullExp[T,Bool]())

  def apply[T,R](dateExp: QuantifiableOrderedExp[T,R], expression: LogicalOperatorsExp[T,Bool]): DateExpAssertionBuilder[T,R] =
    new DateExpAssertionBuilder(dateExp, expression, _ and _)
}

case class DateExpAssertionBuilder[T,R](quantifiableExp: QuantifiableOrderedExp[T,R], expression: LogicalOperatorsExp[T,Bool], operator: (LogicalOperatorsExp[T,Bool], LogicalOperatorsExp[T,Bool]) => LogicalOperatorsExp[T,Bool])
  extends AssertionBuilder[T,DateExpAssertionBuilder[T,R]](expression) {

  def isAfter(date: R): DateExpAssertionBuilder[T,R] =
    isAfter(_ => date)

  def isAfter(date: T => R): DateExpAssertionBuilder[T,R] =
    newWith(quantifiableExp.isGreaterThan(QuantifiableExp(date)))

  def isBefore(date: R): DateExpAssertionBuilder[T,R] =
    isBefore(_ => date)

  def isBefore(date: T => R): DateExpAssertionBuilder[T,R] =
    newWith(quantifiableExp.isLessThan(QuantifiableExp(date)))

  def isAfterOrSameThan(date: R): DateExpAssertionBuilder[T,R] =
    isAfterOrSameThan(_ => date)

  def isAfterOrSameThan(date: T => R): DateExpAssertionBuilder[T,R] =
    newWith(quantifiableExp.isGreaterThanOrEqualTo(QuantifiableExp(date)))

  def isBeforeOrSameThan(date: R): DateExpAssertionBuilder[T,R] =
    isBeforeOrSameThan(_ => date)

  def isBeforeOrSameThan(date: T => R): DateExpAssertionBuilder[T,R] =
    newWith(quantifiableExp.isLessThanOrEqualTo(QuantifiableExp(date)))

  def isBetween(min: R, max: R): DateExpAssertionBuilder[T,R] =
    isBetween(_ => min, _ => max)

  def isBetween(min: T => R, max: T => R): DateExpAssertionBuilder[T,R] =
    newWith(quantifiableExp.isGreaterThanOrEqualTo(QuantifiableExp(min)).and(quantifiableExp.isLessThanOrEqualTo(QuantifiableExp(max))))

  def isEqualTo(date: R): DateExpAssertionBuilder[T,R] =
    isEqualTo(_ => date)

  def isEqualTo(date: T => R): DateExpAssertionBuilder[T,R] =
    newWith(quantifiableExp.isEqualTo(QuantifiableExp(date)))

  override def or: DateExpAssertionBuilder[T, R] =
    DateExpAssertionBuilder(quantifiableExp, expression, _ or _)

  private def newWith(newExpression: LogicalOperatorsExp[T,Bool]) =
    DateExpAssertionBuilder(quantifiableExp, operator.apply(expression, newExpression))
}
