package assertion

import expression._

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
  def apply[T,R](quantifiableExp: QuantifiableOrderedExp[T,R]): QuantifiableExpAssertionBuilder[T,R] =
    new QuantifiableExpAssertionBuilder(quantifiableExp, new NullBooleanExp[T,Bool](), _ and _)
}

case class QuantifiableExpAssertionBuilder[T,R](quantifiableExp: QuantifiableOrderedExp[T,R], expression: BooleanExp[T,Bool], operator: (BooleanExp[T,Bool], BooleanExp[T,Bool]) => BooleanExp[T,Bool]) extends AssertionBuilder[T] {
  def isEqualTo(number: R): BoolExpAssertionBuilder[T,QuantifiableExpAssertionBuilder[T,R]] = isEqualTo(_ => number)
  def isEqualTo(number: T => R): BoolExpAssertionBuilder[T,QuantifiableExpAssertionBuilder[T,R]] = newBoolExp(quantifiableExp.isEqualTo(QuantifiableExp(number)))
  def isGreaterThan(number: R): BoolExpAssertionBuilder[T,QuantifiableExpAssertionBuilder[T,R]] = isGreaterThan(_ => number)
  def isGreaterThan(number: T => R): BoolExpAssertionBuilder[T,QuantifiableExpAssertionBuilder[T,R]] = newBoolExp(quantifiableExp.isGreaterThan(QuantifiableExp(number)))
  def isLessThan(number: R): BoolExpAssertionBuilder[T,QuantifiableExpAssertionBuilder[T,R]] = isLessThan(_ => number)
  def isLessThan(number: T => R): BoolExpAssertionBuilder[T,QuantifiableExpAssertionBuilder[T,R]] = newBoolExp(quantifiableExp.isLessThan(QuantifiableExp(number)))
  def isGreaterThanOrEqualTo(number: => R): BoolExpAssertionBuilder[T,QuantifiableExpAssertionBuilder[T,R]] = isGreaterThanOrEqualTo(_ => number)
  def isGreaterThanOrEqualTo(number: T => R): BoolExpAssertionBuilder[T,QuantifiableExpAssertionBuilder[T,R]] = newBoolExp(quantifiableExp.isGreaterThanOrEqualTo(QuantifiableExp(number)))
  def isLessThanOrEqualTo(number: R): BoolExpAssertionBuilder[T,QuantifiableExpAssertionBuilder[T,R]] = isLessThanOrEqualTo(_ => number)
  def isLessThanOrEqualTo(number: T => R): BoolExpAssertionBuilder[T,QuantifiableExpAssertionBuilder[T,R]] = newBoolExp(quantifiableExp.isLessThanOrEqualTo(QuantifiableExp(number)))
  def isInInclusiveRange(min: R, max: R): BoolExpAssertionBuilder[T,QuantifiableExpAssertionBuilder[T,R]] = isInInclusiveRange(_ => min, _ => max)
  def isInInclusiveRange(min: T => R, max: T => R): BoolExpAssertionBuilder[T,QuantifiableExpAssertionBuilder[T,R]] = newBoolExp(quantifiableExp.isInInclusiveRange(QuantifiableExp(min), QuantifiableExp(max)))
  def isInExclusiveRange(min: R, max: R): BoolExpAssertionBuilder[T,QuantifiableExpAssertionBuilder[T,R]] = isInExclusiveRange(_ => min, _ => max)
  def isInExclusiveRange(min: T => R, max: T => R): BoolExpAssertionBuilder[T,QuantifiableExpAssertionBuilder[T,R]] = newBoolExp(quantifiableExp.isInInclusiveRange(QuantifiableExp(min), QuantifiableExp(max)))
  private def newBoolExp(anExpression: BooleanExp[T,Bool]): BoolExpAssertionBuilder[T, QuantifiableExpAssertionBuilder[T,R]] =
    BoolExpAssertionBuilder(operator(expression, anExpression), (newExpression,newOperator) => QuantifiableExpAssertionBuilder(quantifiableExp, newExpression, newOperator))
}
