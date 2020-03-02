package org.validify.assertion

import org.validify.expression._

import scala.reflect.{ClassTag,classTag}

object QuantifiableExpAssertionBuilder {

  import QuantifiableOrderedExp._

  def fromIntConstant[T](int: Int): QuantifiableExpAssertionBuilder[T,Int] =
    QuantifiableExpAssertionBuilder(intConstant(int))

  def fromIntVariable[T](int: T => Int): QuantifiableExpAssertionBuilder[T,Int] =
    QuantifiableExpAssertionBuilder(intVariable(int))

  def fromDoubleConstant[T](double: Double): QuantifiableExpAssertionBuilder[T,Double] =
    QuantifiableExpAssertionBuilder(doubleConstant(double))

  def fromDoubleVariable[T](double: T => Double): QuantifiableExpAssertionBuilder[T,Double] =
    QuantifiableExpAssertionBuilder(doubleVariable(double))

  def fromLongConstant[T](long: Long): QuantifiableExpAssertionBuilder[T,Long] =
    QuantifiableExpAssertionBuilder(longConstant(long))

  def fromLongVariable[T](long: T => Long): QuantifiableExpAssertionBuilder[T,Long] =
    QuantifiableExpAssertionBuilder(longVariable(long))

  def fromBigDecimalConstant[T](bigDecimal: BigDecimal): QuantifiableExpAssertionBuilder[T,BigDecimal] =
    QuantifiableExpAssertionBuilder(bigDecimalConstant(bigDecimal))

  def fromBigDecimalVariable[T](bigDecimal: T => BigDecimal): QuantifiableExpAssertionBuilder[T,BigDecimal] =
    QuantifiableExpAssertionBuilder(bigDecimalVariable(bigDecimal))

  def apply[T,R: ClassTag](quantifiableExp: QuantifiableOrderedExp[T,R]): QuantifiableExpAssertionBuilder[T,R] =
    QuantifiableExpAssertionBuilder(quantifiableExp, new NullExp[T,Bool]())

  def apply[T,R: ClassTag](quantifiableExp: QuantifiableOrderedExp[T,R], expression: ComposableBooleanExp[T]): QuantifiableExpAssertionBuilder[T,R] =
    new QuantifiableExpAssertionBuilder(quantifiableExp, expression, _ and _)
}

case class QuantifiableExpAssertionBuilder[T,R: ClassTag](quantifiableExp: QuantifiableOrderedExp[T,R], expression: ComposableBooleanExp[T], operator: (ComposableBooleanExp[T], ComposableBooleanExp[T]) => ComposableBooleanExp[T])
  extends AssertionBuilder[T,QuantifiableExpAssertionBuilder[T,R]](expression) {

  def isEqualTo(number: R): QuantifiableExpAssertionBuilder[T,R] =
    isEqualTo(_ => number)

  def isEqualTo(number: T => R): QuantifiableExpAssertionBuilder[T,R] =
    newWith(quantifiableExp.isEqualTo(QuantifiableExp(number)))

  def isGreaterThan(number: R): QuantifiableExpAssertionBuilder[T,R] =
    isGreaterThan(_ => number)

  def isGreaterThan(number: T => R): QuantifiableExpAssertionBuilder[T,R] =
    newWith(quantifiableExp.isGreaterThan(QuantifiableExp(number)))

  def isLessThan(number: R): QuantifiableExpAssertionBuilder[T,R] =
    isLessThan(_ => number)

  def isLessThan(number: T => R): QuantifiableExpAssertionBuilder[T,R] =
    newWith(quantifiableExp.isLessThan(QuantifiableExp(number)))

  def isGreaterThanOrEqualTo(number: => R): QuantifiableExpAssertionBuilder[T,R] =
    isGreaterThanOrEqualTo(_ => number)

  def isGreaterThanOrEqualTo(number: T => R): QuantifiableExpAssertionBuilder[T,R] =
    newWith(quantifiableExp.isGreaterThanOrEqualTo(QuantifiableExp(number)))

  def isLessThanOrEqualTo(number: R): QuantifiableExpAssertionBuilder[T,R] =
    isLessThanOrEqualTo(_ => number)

  def isLessThanOrEqualTo(number: T => R): QuantifiableExpAssertionBuilder[T,R] =
    newWith(quantifiableExp.isLessThanOrEqualTo(QuantifiableExp(number)))

  def isInInclusiveRange(min: R, max: R): QuantifiableExpAssertionBuilder[T,R] =
    isInInclusiveRange(_ => min, _ => max)

  def isInInclusiveRange(min: T => R, max: T => R): QuantifiableExpAssertionBuilder[T,R] =
    newWith(quantifiableExp.isInInclusiveRange(QuantifiableExp(min), QuantifiableExp(max)))

  def isInExclusiveRange(min: R, max: R): QuantifiableExpAssertionBuilder[T,R] =
    isInExclusiveRange(_ => min, _ => max)

  def isInExclusiveRange(min: T => R, max: T => R): QuantifiableExpAssertionBuilder[T,R] =
    newWith(quantifiableExp.isInInclusiveRange(QuantifiableExp(min), QuantifiableExp(max)))

  def isPercentage: QuantifiableExpAssertionBuilder[T,R] = {
    val clazz = classTag[R].runtimeClass
    if (clazz == classOf[BigDecimal]) isInInclusiveRange(BigDecimal(0).asInstanceOf[R], BigDecimal(100).asInstanceOf[R])
    else if (clazz == classOf[Double]) isInInclusiveRange(0D.asInstanceOf[R], 100D.asInstanceOf[R])
    else if (clazz == classOf[Long]) isInInclusiveRange(0L.asInstanceOf[R], 100L.asInstanceOf[R])
    else isInInclusiveRange(0.asInstanceOf[R], 100.asInstanceOf[R])
  }

  override def or: QuantifiableExpAssertionBuilder[T, R] =
    QuantifiableExpAssertionBuilder(quantifiableExp, expression, _ or _)

  private def newWith(newExpression: ComposableBooleanExp[T]) =
    QuantifiableExpAssertionBuilder(quantifiableExp, operator.apply(expression, newExpression))
}
