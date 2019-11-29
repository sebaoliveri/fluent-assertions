package assertion

import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}
import expression._

object OptionalDateExpAssertionBuilder {
  import DateExp._

  def fromMaybeInstantConstant[T,R](maybeInstant: Option[Instant]): OptionalDateExpAssertionBuilder[T,Instant] =
    fromMaybeInstantVariable(_ => maybeInstant)

  def fromMaybeZonedDateTimeConstant[T,R](maybeZonedDateTime: Option[ZonedDateTime]): OptionalDateExpAssertionBuilder[T,ZonedDateTime] =
    fromMaybeZonedDateTimeVariable(_ => maybeZonedDateTime)

  def fromMaybeLocalDateConstant[T,R](maybeLocalDate: Option[LocalDate]): OptionalDateExpAssertionBuilder[T,LocalDate] =
    fromMaybeLocalDateVariable(_ => maybeLocalDate)

  def fromMaybeLocalDateTimeConstant[T,R](maybeLocalDateTime: Option[LocalDateTime]): OptionalDateExpAssertionBuilder[T,LocalDateTime] =
    fromMaybeLocalDateTimeVariable(_ => maybeLocalDateTime)

  def fromMaybeInstantVariable[T,R](maybeInstant: T => Option[Instant]): OptionalDateExpAssertionBuilder[T,Instant] =
    OptionalDateExpAssertionBuilder(maybeInstantVariable(maybeInstant))

  def fromMaybeZonedDateTimeVariable[T,R](maybeZonedDateTime: T => Option[ZonedDateTime]): OptionalDateExpAssertionBuilder[T,ZonedDateTime] =
    OptionalDateExpAssertionBuilder(maybeZonedDateTimeVariable(maybeZonedDateTime))

  def fromMaybeLocalDateVariable[T,R](maybeLocalDate: T => Option[LocalDate]): OptionalDateExpAssertionBuilder[T,LocalDate] =
    OptionalDateExpAssertionBuilder(maybeLocalDateVariable(maybeLocalDate))

  def fromMaybeLocalDateTimeVariable[T,R](maybeLocalDateTime: T => Option[LocalDateTime]): OptionalDateExpAssertionBuilder[T,LocalDateTime] =
    OptionalDateExpAssertionBuilder(maybeLocalDateTimeVariable(maybeLocalDateTime))

  def apply[T,R](optionalExp: OptionalExp[T,Ordered[R]]): OptionalDateExpAssertionBuilder[T,R] =
    OptionalDateExpAssertionBuilder(optionalExp, new NullBooleanExp[T,Bool]())

  def apply[T,R](optionalExp: OptionalExp[T,Ordered[R]], expression: BooleanExp[T,Bool]): OptionalDateExpAssertionBuilder[T,R] =
    new OptionalDateExpAssertionBuilder(optionalExp, new NullBooleanExp[T,Bool](), _ and _)
}

case class OptionalDateExpAssertionBuilder[T,R](optionExp: OptionalExp[T,Ordered[R]], expression: BooleanExp[T,Bool], operator: (BooleanExp[T,Bool], BooleanExp[T,Bool]) => BooleanExp[T,Bool])
  extends BoolExpAssertionBuilder[T,OptionalDateExpAssertionBuilder[T,R]](expression) {

  def isDefined: OptionalDateExpAssertionBuilder[T,R] =
    OptionalDateExpAssertionBuilder(optionExp, operator.apply(expression, IsDefinedExp[T,Ordered[R]](optionExp)))

  def wouldBeAfter(date: R): OptionalDateExpAssertionBuilder[T,R] =
    wouldBeAfter(_ => date)

  def wouldBeAfter(date: T => R): OptionalDateExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isGreaterThan(QuantifiableExp(date)))

  def wouldBeBefore(date: R): OptionalDateExpAssertionBuilder[T,R] =
    wouldBeBefore(_ => date)

  def wouldBeBefore(date: T => R): OptionalDateExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isLessThan(QuantifiableExp(date)))

  def wouldBeAfterOrSameThan(date: R): OptionalDateExpAssertionBuilder[T,R] =
    wouldBeAfterOrSameThan(_ => date)

  def wouldBeAfterOrSameThan(date: T => R): OptionalDateExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isGreaterThanOrEqualTo(QuantifiableExp(date)))

  def wouldBeBeforeOrSameThan(date: R): OptionalDateExpAssertionBuilder[T,R] =
    wouldBeBeforeOrSameThan(_ => date)

  def wouldBeBeforeOrSameThan(date: T => R): OptionalDateExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isLessThanOrEqualTo(QuantifiableExp(date)))

  def wouldBeBetween(min: R, max: R): OptionalDateExpAssertionBuilder[T,R] =
    wouldBeBetween(_ => min, _ => max)

  def wouldBeBetween(min: T => R, max: T => R): OptionalDateExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isInInclusiveRange(QuantifiableExp(min), QuantifiableExp(max)))

  def wouldBeEqualTo(date: R): OptionalDateExpAssertionBuilder[T, R] =
    wouldBeEqualTo(_ => date)

  def wouldBeEqualTo(date: T => R): OptionalDateExpAssertionBuilder[T, R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isEqualTo(QuantifiableExp(date)))

  override def or: OptionalDateExpAssertionBuilder[T, R] =
    OptionalDateExpAssertionBuilder(optionExp, expression, _ or _)

  private def newWith(newExpression: Ordered[R] => BooleanExp[T,Bool]) =
    OptionalDateExpAssertionBuilder(optionExp,
      operator.apply(expression, OptionalBoolExp[T,Ordered[R]](optionExp, newExpression)))
}
