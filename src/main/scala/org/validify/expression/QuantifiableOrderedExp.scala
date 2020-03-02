package org.validify.expression

import scala.runtime.{RichDouble, RichInt, RichLong}

object QuantifiableOrderedExp {

  def intConstant[T](int: Int): QuantifiableOrderedExp[T,Int] =
    intVariable(_ => int)

  def intVariable[T](f: T => Int): QuantifiableOrderedExp[T,Int] =
    QuantifiableOrderedExp(f.andThen(new RichInt(_)))

  def longConstant[T](long: Long): QuantifiableOrderedExp[T,Long] = longVariable(_ => long)

  def longVariable[T](f: T => Long): QuantifiableOrderedExp[T,Long] =
    QuantifiableOrderedExp(f.andThen(new RichLong(_)))

  def doubleConstant[T](double: Double): QuantifiableOrderedExp[T,Double] =
    doubleVariable(_ => double)

  def doubleVariable[T](f: T => Double): QuantifiableOrderedExp[T,Double] =
    QuantifiableOrderedExp(f.andThen(new RichDouble(_)))

  def bigDecimalConstant[T](bigDecimal: BigDecimal): QuantifiableOrderedExp[T,BigDecimal] =
    bigDecimalVariable(_ => bigDecimal)

  def bigDecimalVariable[T](f: T => Ordered[BigDecimal]): QuantifiableOrderedExp[T,BigDecimal] =
    QuantifiableOrderedExp(f)

  def maybeIntConstant[T](maybeInt: Option[Int]): OptionalExp[T, Ordered[Int]] =
    maybeIntVariable(_ => maybeInt)

  def maybeIntVariable[T](maybeInt: T => Option[Int]): OptionalExp[T,Ordered[Int]] =
    OptionalExp(maybeInt.andThen(_.map(new RichInt(_))))

  def maybeLongConstant[T](maybeLong: Option[Long]): OptionalExp[T, Ordered[Long]] =
    maybeLongVariable(_ => maybeLong)

  def maybeLongVariable[T](maybeLong: T => Option[Long]): OptionalExp[T, Ordered[Long]] =
    OptionalExp(maybeLong.andThen(_.map(new RichLong(_))))

  def maybeDoubleConstant[T](maybeDouble: Option[Double]): OptionalExp[T, Ordered[Double]] =
    maybeDoubleVariable(_ => maybeDouble)

  def maybeDoubleVariable[T](maybeDouble: T => Option[Double]): OptionalExp[T, Ordered[Double]] =
    OptionalExp(maybeDouble.andThen(_.map(new RichDouble(_))))

  def maybeBigDecimalConstant[T](maybeBigDecimal: Option[BigDecimal]): OptionalExp[T, Ordered[BigDecimal]] =
    maybeBigDecimalVariable(_ => maybeBigDecimal)

  def maybeBigDecimalVariable[T](maybeBigDecimal: T => Option[BigDecimal]): OptionalExp[T, Ordered[BigDecimal]] =
    OptionalExp(maybeBigDecimal)
}

case class QuantifiableOrderedExp[T,R](func: T => Ordered[R]) extends AnyExp[T,Ordered[R]](func) {

  def isGreaterThan(anotherQuantifiableExp: QuantifiableExp[T,R]): IsGreaterThanExp[T,R] =
    IsGreaterThanExp(this, anotherQuantifiableExp)

  def isLessThan(anotherQuantifiableExp: QuantifiableExp[T,R]): IsLessThanExp[T, R] =
    IsLessThanExp(this, anotherQuantifiableExp)

  def isGreaterThanOrEqualTo(anotherQuantifiableExp: QuantifiableExp[T,R]): ComposableBooleanExp[T] =
    isGreaterThan(anotherQuantifiableExp).or(isEqualTo(anotherQuantifiableExp))

  def isLessThanOrEqualTo(anotherQuantifiableExp: QuantifiableExp[T,R]): ComposableBooleanExp[T] =
    isLessThan(anotherQuantifiableExp).or(isEqualTo(anotherQuantifiableExp))

  def isInInclusiveRange(min: QuantifiableExp[T,R], max: QuantifiableExp[T,R]): ComposableBooleanExp[T] =
    isGreaterThanOrEqualTo(min).and(isLessThanOrEqualTo(max))

  def isInExclusiveRange(min: QuantifiableExp[T,R], max: QuantifiableExp[T,R]): ComposableBooleanExp[T]  =
    isGreaterThan(min).and(isLessThan(max))

  def isEqualTo(anotherQuantifiableExp: QuantifiableExp[T,R]): IsQuantifiableEqualToExp[T, R] =
    IsQuantifiableEqualToExp(this, anotherQuantifiableExp)

  def isNotEqualTo(anotherQuantifiableExp: QuantifiableExp[T,R]): NotExp[T] =
    NotExp(IsQuantifiableEqualToExp(this, anotherQuantifiableExp))
}

case class QuantifiableExp[T,R](func: T => R) extends AnyExp[T,R](func)

case class IsGreaterThanExp[T,R](left: QuantifiableOrderedExp[T,R], right: QuantifiableExp[T,R]) extends ComposableBooleanExp[T] {

  override def evaluate(context: T): Bool =
    Bool(left.evaluate(context) > right.evaluate(context))
}

case class IsLessThanExp[T,R](left: QuantifiableOrderedExp[T,R], right: QuantifiableExp[T,R]) extends ComposableBooleanExp[T] {

  override def evaluate(context: T): Bool =
    Bool(left.evaluate(context) < right.evaluate(context))
}

case class IsQuantifiableEqualToExp[T,R](left: QuantifiableOrderedExp[T,R], right: QuantifiableExp[T,R]) extends ComposableBooleanExp[T] {

  override def evaluate(context: T): Bool =
    Bool(left.evaluate(context).compareTo(right.evaluate(context)) == 0)
}
