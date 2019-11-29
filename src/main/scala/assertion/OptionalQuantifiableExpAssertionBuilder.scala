package assertion

import expression._

object OptionalQuantifiableExpAssertionBuilder {

  import QuantifiableOrderedExp._

  def fromMaybeIntConstant[T](maybeInt: Option[Int]): OptionalQuantifiableExpAssertionBuilder[T,Int] =
    fromMaybeIntVariable(_ => maybeInt)

  def fromMaybeIntVariable[T](maybeInt: T => Option[Int]): OptionalQuantifiableExpAssertionBuilder[T,Int] =
    OptionalQuantifiableExpAssertionBuilder(maybeIntVariable(maybeInt))

  def fromMaybeDoubleConstant[T](maybeDouble: Option[Double]): OptionalQuantifiableExpAssertionBuilder[T,Double] =
    fromMaybeDoubleVariable(_ => maybeDouble)

  def fromMaybeDoubleVariable[T](maybeDouble: T => Option[Double]): OptionalQuantifiableExpAssertionBuilder[T,Double] =
    OptionalQuantifiableExpAssertionBuilder(maybeDoubleVariable(maybeDouble))

  def fromMaybeLongConstant[T](maybeLong: Option[Long]): OptionalQuantifiableExpAssertionBuilder[T,Long] =
    fromMaybeLongVariable(_ => maybeLong)

  def fromMaybeLongVariable[T](maybeLong: T => Option[Long]): OptionalQuantifiableExpAssertionBuilder[T,Long] =
    OptionalQuantifiableExpAssertionBuilder(maybeLongVariable(maybeLong))

  def fromMaybeBigDecimalConstant[T](maybeBigDecimal: Option[BigDecimal]): OptionalQuantifiableExpAssertionBuilder[T,BigDecimal] =
    fromMaybeBigDecimalVariable(_ => maybeBigDecimal)

  def fromMaybeBigDecimalVariable[T](maybeBigDecimal: T => Option[BigDecimal]): OptionalQuantifiableExpAssertionBuilder[T,BigDecimal] =
    OptionalQuantifiableExpAssertionBuilder(maybeBigDecimalVariable(maybeBigDecimal))

  def apply[T,R](optionalExp: OptionalExp[T,Ordered[R]]): OptionalQuantifiableExpAssertionBuilder[T,R] =
    OptionalQuantifiableExpAssertionBuilder(optionalExp, new NullBooleanExp[T,Bool]())

  def apply[T,R](optionalExp: OptionalExp[T,Ordered[R]], expression: BooleanExp[T,Bool]): OptionalQuantifiableExpAssertionBuilder[T,R] =
    new OptionalQuantifiableExpAssertionBuilder(optionalExp, expression, _ and _)
}

case class OptionalQuantifiableExpAssertionBuilder[T,R](optionExp: OptionalExp[T,Ordered[R]], expression: BooleanExp[T,Bool], operator: (BooleanExp[T,Bool], BooleanExp[T,Bool]) => BooleanExp[T,Bool])
  extends BoolExpAssertionBuilder[T,OptionalQuantifiableExpAssertionBuilder[T,R]](expression) {

  def isDefined: OptionalQuantifiableExpAssertionBuilder[T,R] =
    OptionalQuantifiableExpAssertionBuilder(optionExp, operator(expression, IsDefinedExp[T,Ordered[R]](optionExp)))

  def wouldBeEqualTo(maybeQuantity: R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    wouldBeEqualTo(_ => maybeQuantity)

  def wouldBeEqualTo(maybeQuantity: T => R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isEqualTo(QuantifiableExp(maybeQuantity)))

  def wouldBeGreaterThan(maybeQuantity: R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    wouldBeGreaterThan(_ => maybeQuantity)

  def wouldBeGreaterThan(maybeQuantity: T => R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isGreaterThan(QuantifiableExp(maybeQuantity)))

  def wouldBeLessThan(maybeQuantity: R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    wouldBeLessThan(_ => maybeQuantity)

  def wouldBeLessThan(maybeQuantity: T => R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isLessThan(QuantifiableExp(maybeQuantity)))

  def wouldBeGreaterThanOrEqualTo(maybeQuantity: R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    wouldBeGreaterThanOrEqualTo(_ => maybeQuantity)

  def wouldBeGreaterThanOrEqualTo(maybeQuantity: T => R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isGreaterThanOrEqualTo(QuantifiableExp(maybeQuantity)))

  def wouldBeLessThanOrEqualTo(maybeQuantity: R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    wouldBeLessThanOrEqualTo(_ => maybeQuantity)

  def wouldBeLessThanOrEqualTo(maybeQuantity: T => R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isLessThanOrEqualTo(QuantifiableExp(maybeQuantity)))

  def wouldBeInInclusiveRange(min: R, max: R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    wouldBeInInclusiveRange(_ => min, _ => max)

  def wouldBeInInclusiveRange(min: T => R, max: T => R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isInInclusiveRange(QuantifiableExp(min), QuantifiableExp(max)))

  def wouldBeInExclusiveRange(min: R, max: R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    wouldBeInExclusiveRange(_ => min, _ => max)

  def wouldBeInExclusiveRange(min: T => R, max: T => R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isInExclusiveRange(QuantifiableExp(min), QuantifiableExp(max)))

  override def or: OptionalQuantifiableExpAssertionBuilder[T, R] =
    OptionalQuantifiableExpAssertionBuilder(optionExp, expression, _ or _)

  private def newWith(newExpression: Ordered[R] => BooleanExp[T,Bool]) =
    OptionalQuantifiableExpAssertionBuilder(optionExp,
      operator.apply(expression, OptionalBoolExp[T,Ordered[R]](optionExp, newExpression)))
}
