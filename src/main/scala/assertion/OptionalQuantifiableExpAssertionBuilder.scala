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
    OptionalQuantifiableExpAssertionBuilder(optionalExp, new NullExp[T,Bool]())

  def apply[T,R](optionalExp: OptionalExp[T,Ordered[R]], expression: BoolExpBehaviour[T]): OptionalQuantifiableExpAssertionBuilder[T,R] =
    new OptionalQuantifiableExpAssertionBuilder(optionalExp, expression, _ and _)
}

case class OptionalQuantifiableExpAssertionBuilder[T,R](optionExp: OptionalExp[T,Ordered[R]], expression: BoolExpBehaviour[T], operator: (BoolExpBehaviour[T], BoolExpBehaviour[T]) => BoolExpBehaviour[T])
  extends AssertionBuilder[T,OptionalQuantifiableExpAssertionBuilder[T,R]](expression) {

  def isDefined: OptionalQuantifiableExpAssertionBuilder[T,R] =
    OptionalQuantifiableExpAssertionBuilder(optionExp, operator(expression, IsDefinedExp[T,Ordered[R]](optionExp)))

  def isEqualTo(maybeQuantity: R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    isEqualTo(_ => maybeQuantity)

  def isEqualTo(maybeQuantity: T => R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isEqualTo(QuantifiableExp(maybeQuantity)))

  def isGreaterThan(maybeQuantity: R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    isGreaterThan(_ => maybeQuantity)

  def isGreaterThan(maybeQuantity: T => R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isGreaterThan(QuantifiableExp(maybeQuantity)))

  def isLessThan(maybeQuantity: R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    isLessThan(_ => maybeQuantity)

  def isLessThan(maybeQuantity: T => R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isLessThan(QuantifiableExp(maybeQuantity)))

  def isGreaterThanOrEqualTo(maybeQuantity: R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    isGreaterThanOrEqualTo(_ => maybeQuantity)

  def isGreaterThanOrEqualTo(maybeQuantity: T => R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isGreaterThanOrEqualTo(QuantifiableExp(maybeQuantity)))

  def isLessThanOrEqualTo(maybeQuantity: R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    isLessThanOrEqualTo(_ => maybeQuantity)

  def isLessThanOrEqualTo(maybeQuantity: T => R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isLessThanOrEqualTo(QuantifiableExp(maybeQuantity)))

  def isInInclusiveRange(min: R, max: R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    isInInclusiveRange(_ => min, _ => max)

  def isInInclusiveRange(min: T => R, max: T => R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isInInclusiveRange(QuantifiableExp(min), QuantifiableExp(max)))

  def isInExclusiveRange(min: R, max: R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    isInExclusiveRange(_ => min, _ => max)

  def isInExclusiveRange(min: T => R, max: T => R): OptionalQuantifiableExpAssertionBuilder[T,R] =
    newWith(ordered => QuantifiableOrderedExp({_:T => ordered}).isInExclusiveRange(QuantifiableExp(min), QuantifiableExp(max)))

  def isPercentage: OptionalQuantifiableExpAssertionBuilder[T, R] =
    isInInclusiveRange(0.asInstanceOf[R], 100.asInstanceOf[R])

  override def or: OptionalQuantifiableExpAssertionBuilder[T, R] =
    OptionalQuantifiableExpAssertionBuilder(optionExp, expression, _ or _)

  private def newWith(newExpression: Ordered[R] => BoolExpBehaviour[T]) =
    OptionalQuantifiableExpAssertionBuilder(optionExp,
      operator.apply(expression, OptionalBoolExp[T,Ordered[R]](optionExp, newExpression)))
}
