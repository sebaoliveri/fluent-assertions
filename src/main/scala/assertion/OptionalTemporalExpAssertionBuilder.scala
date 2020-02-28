package assertion

import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}
import expression._

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

  def apply[T,R](optionalExp: OptionalExp[T,Ordered[R]], expression: BoolExpBehaviour[T]): OptionalTemporalExpAssertionBuilder[T,R] =
    new OptionalTemporalExpAssertionBuilder(optionalExp, new NullExp[T,Bool](), _ and _)
}

case class OptionalTemporalExpAssertionBuilder[T,R](optionExp: OptionalExp[T,Ordered[R]], expression: BoolExpBehaviour[T], operator: (BoolExpBehaviour[T], BoolExpBehaviour[T]) => BoolExpBehaviour[T])
  extends AssertionBuilder[T,OptionalTemporalExpAssertionBuilder[T,R]](expression) {

  def isDefined: OptionalTemporalExpAssertionBuilder[T,R] =
    OptionalTemporalExpAssertionBuilder(optionExp, operator.apply(expression, IsDefinedExp[T,Ordered[R]](optionExp)))

  def wouldBeAfter(date: R): OptionalTemporalExpAssertionBuilder[T,R] =
    wouldBeAfter(_ => date)

  def wouldBeAfter(date: T => R): OptionalTemporalExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isGreaterThan(QuantifiableExp(date)))

  def wouldBeBefore(date: R): OptionalTemporalExpAssertionBuilder[T,R] =
    wouldBeBefore(_ => date)

  def wouldBeBefore(date: T => R): OptionalTemporalExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isLessThan(QuantifiableExp(date)))

  def wouldBeAfterOrSameThan(date: R): OptionalTemporalExpAssertionBuilder[T,R] =
    wouldBeAfterOrSameThan(_ => date)

  def wouldBeAfterOrSameThan(date: T => R): OptionalTemporalExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isGreaterThanOrEqualTo(QuantifiableExp(date)))

  def wouldBeBeforeOrSameThan(date: R): OptionalTemporalExpAssertionBuilder[T,R] =
    wouldBeBeforeOrSameThan(_ => date)

  def wouldBeBeforeOrSameThan(date: T => R): OptionalTemporalExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isLessThanOrEqualTo(QuantifiableExp(date)))

  def wouldBeBetween(min: R, max: R): OptionalTemporalExpAssertionBuilder[T,R] =
    wouldBeBetween(_ => min, _ => max)

  def wouldBeBetween(min: T => R, max: T => R): OptionalTemporalExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isInInclusiveRange(QuantifiableExp(min), QuantifiableExp(max)))

  def wouldBeEqualTo(date: R): OptionalTemporalExpAssertionBuilder[T, R] =
    wouldBeEqualTo(_ => date)

  def wouldBeEqualTo(date: T => R): OptionalTemporalExpAssertionBuilder[T, R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isEqualTo(QuantifiableExp(date)))

  override def or: OptionalTemporalExpAssertionBuilder[T, R] =
    OptionalTemporalExpAssertionBuilder(optionExp, expression, _ or _)

  private def newWith(newExpression: Ordered[R] => BoolExpBehaviour[T]) =
    OptionalTemporalExpAssertionBuilder(optionExp,
      operator.apply(expression, OptionalBoolExp[T,Ordered[R]](optionExp, newExpression)))
}
