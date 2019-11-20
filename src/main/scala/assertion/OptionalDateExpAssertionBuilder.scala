package assertion

import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}
import expression._

object OptionalDateExpAssertionBuilder {
  import DateExp._
  def fromMaybeInstantConstant[T,R](maybeInstant: Option[Instant]): OptionalDateExpAssertionBuilder[T,Instant] = fromMaybeInstantVariable(_ => maybeInstant)
  def fromMaybeZonedDateTimeConstant[T,R](maybeZonedDateTime: Option[ZonedDateTime]): OptionalDateExpAssertionBuilder[T,ZonedDateTime] = fromMaybeZonedDateTimeVariable(_ => maybeZonedDateTime)
  def fromMaybeLocalDateConstant[T,R](maybeLocalDate: Option[LocalDate]): OptionalDateExpAssertionBuilder[T,LocalDate] = fromMaybeLocalDateVariable(_ => maybeLocalDate)
  def fromMaybeLocalDateTimeConstant[T,R](maybeLocalDateTime: Option[LocalDateTime]): OptionalDateExpAssertionBuilder[T,LocalDateTime] = fromMaybeLocalDateTimeVariable(_ => maybeLocalDateTime)
  def fromMaybeInstantVariable[T,R](maybeInstant: T => Option[Instant]): OptionalDateExpAssertionBuilder[T,Instant] = OptionalDateExpAssertionBuilder(maybeInstantVariable(maybeInstant))
  def fromMaybeZonedDateTimeVariable[T,R](maybeZonedDateTime: T => Option[ZonedDateTime]): OptionalDateExpAssertionBuilder[T,ZonedDateTime] = OptionalDateExpAssertionBuilder(maybeZonedDateTimeVariable(maybeZonedDateTime))
  def fromMaybeLocalDateVariable[T,R](maybeLocalDate: T => Option[LocalDate]): OptionalDateExpAssertionBuilder[T,LocalDate] = OptionalDateExpAssertionBuilder(maybeLocalDateVariable(maybeLocalDate))
  def fromMaybeLocalDateTimeVariable[T,R](maybeLocalDateTime: T => Option[LocalDateTime]): OptionalDateExpAssertionBuilder[T,LocalDateTime] = OptionalDateExpAssertionBuilder(maybeLocalDateTimeVariable(maybeLocalDateTime))
  def apply[T,R](optionalExp: OptionalExp[T,Ordered[R]]): OptionalDateExpAssertionBuilder[T,R] = new OptionalDateExpAssertionBuilder(optionalExp, new NullBooleanExp[T,Bool](), _ and _)
}

case class OptionalDateExpAssertionBuilder[T,R](optionExp: OptionalExp[T,Ordered[R]], expression: BooleanExp[T,Bool], operator: (BooleanExp[T,Bool], BooleanExp[T,Bool]) => BooleanExp[T,Bool]) extends AssertionBuilder[T] {
  private def newBoolExp(anExpression: BooleanExp[T,Bool]): BoolExpAssertionBuilder[T, OptionalDateExpAssertionBuilder[T,R]] =
    BoolExpAssertionBuilder(operator(expression, anExpression), (newExpression,newOperator) => OptionalDateExpAssertionBuilder(optionExp, newExpression, newOperator))
  def isDefined: BoolExpAssertionBuilder[T,DateExpAssertionBuilder[T,R]] =
    BoolExpAssertionBuilder(
      IfDefinedExp[T,Ordered[R]](optionExp),
      (newExpression:BooleanExp[T,Bool], newOperator:((BooleanExp[T,Bool],BooleanExp[T,Bool])=>BooleanExp[T,Bool])) =>
        DateExpAssertionBuilder(QuantifiableOrderedExp(optionExp.func.andThen(_.get)), newExpression, newOperator))
  def wouldBeAfter(date: R): BoolExpAssertionBuilder[T,OptionalDateExpAssertionBuilder[T,R]] = wouldBeAfter(_ => date)
  def wouldBeAfter(date: T => R): BoolExpAssertionBuilder[T,OptionalDateExpAssertionBuilder[T,R]] = newBoolExp(OptionalBoolExp[T,Ordered[R]](optionExp, ordered => QuantifiableOrderedExp({_:T => ordered}).isGreaterThan(QuantifiableExp(date))))
  def wouldBeBefore(date: R): BoolExpAssertionBuilder[T,OptionalDateExpAssertionBuilder[T,R]] = wouldBeBefore(_ => date)
  def wouldBeBefore(date: T => R): BoolExpAssertionBuilder[T,OptionalDateExpAssertionBuilder[T,R]] = newBoolExp(OptionalBoolExp[T,Ordered[R]](optionExp, ordered => QuantifiableOrderedExp({_:T => ordered}).isLessThan(QuantifiableExp(date))))
  def wouldBeAfterOrSameThan(date: R): BoolExpAssertionBuilder[T,OptionalDateExpAssertionBuilder[T,R]] = wouldBeAfterOrSameThan(_ => date)
  def wouldBeAfterOrSameThan(date: T => R): BoolExpAssertionBuilder[T,OptionalDateExpAssertionBuilder[T,R]] = newBoolExp(OptionalBoolExp[T,Ordered[R]](optionExp, ordered => QuantifiableOrderedExp({_:T => ordered}).isGreaterThanOrEqualTo(QuantifiableExp(date))))
  def wouldBeBeforeOrSameThan(date: R): BoolExpAssertionBuilder[T,OptionalDateExpAssertionBuilder[T,R]] = wouldBeBeforeOrSameThan(_ => date)
  def wouldBeBeforeOrSameThan(date: T => R): BoolExpAssertionBuilder[T,OptionalDateExpAssertionBuilder[T,R]] = newBoolExp(OptionalBoolExp[T,Ordered[R]](optionExp, ordered => QuantifiableOrderedExp({_:T => ordered}).isLessThanOrEqualTo(QuantifiableExp(date))))
  def wouldBeBetween(min: R, max: R): BoolExpAssertionBuilder[T,OptionalDateExpAssertionBuilder[T,R]] = wouldBeBetween(_ => min, _ => max)
  def wouldBeBetween(min: T => R, max: T => R): BoolExpAssertionBuilder[T,OptionalDateExpAssertionBuilder[T,R]] = newBoolExp(OptionalBoolExp[T,Ordered[R]](optionExp, ordered => QuantifiableOrderedExp({_:T => ordered}).isInInclusiveRange(QuantifiableExp(min), QuantifiableExp(max))))
  def wouldBeEqualTo(date: R): BoolExpAssertionBuilder[T, OptionalDateExpAssertionBuilder[T, R]] = wouldBeEqualTo(_ => date)
  def wouldBeEqualTo(date: T => R): BoolExpAssertionBuilder[T, OptionalDateExpAssertionBuilder[T, R]] = newBoolExp(OptionalBoolExp[T,Ordered[R]](optionExp, ordered => QuantifiableOrderedExp({_:T => ordered}).isEqualTo(QuantifiableExp(date))))
}
