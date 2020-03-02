package org.validify.extension

import org.validify.expression.{ComposableBooleanExp, QuantifiableExp, QuantifiableOrderedExp}

object QuantifiableExt {

  import QuantifiableOrderedExp._

  implicit class IntExtensions(int: Int) {

    def isGreaterThanExp(anotherInt: Int): ComposableBooleanExp[Unit] =
      intConstant(int).isGreaterThan(QuantifiableExp(_ => anotherInt))

    def isLessThanExp(anotherInt: Int): ComposableBooleanExp[Unit] =
      intConstant(int).isLessThan(QuantifiableExp(_ => anotherInt))

    def isGreaterThanOrEqualToExp(anotherInt: Int): ComposableBooleanExp[Unit] =
      intConstant(int).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherInt))

    def isLessThanOrEqualToExp(anotherInt: Int): ComposableBooleanExp[Unit] =
      intConstant(int).isLessThanOrEqualTo(QuantifiableExp(_ => anotherInt))

    def isInInclusiveRangeExp(min: Int, max: Int): ComposableBooleanExp[Unit] =
      intConstant(int).isInInclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isInExclusiveRangeExp(min: Int, max: Int): ComposableBooleanExp[Unit] =
      intConstant(int).isInExclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isPercentageExp: ComposableBooleanExp[Unit] =
      isInInclusiveRangeExp(0, 100)
  }

  implicit class LongExtensions(long: Long) {

    def isGreaterThanExp(anotherLong: Long): ComposableBooleanExp[Unit] =
      longConstant(long).isGreaterThan(QuantifiableExp(_ => anotherLong))

    def isLessThanExp(anotherLong: Long): ComposableBooleanExp[Unit] =
      longConstant(long).isLessThan(QuantifiableExp(_ => anotherLong))

    def isGreaterThanOrEqualToExp(anotherLong: Long): ComposableBooleanExp[Unit] =
      longConstant(long).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherLong))

    def isLessThanOrEqualToExp(anotherLong: Long): ComposableBooleanExp[Unit] =
      longConstant(long).isLessThanOrEqualTo(QuantifiableExp(_ => anotherLong))

    def isInInclusiveRangeExp(min: Long, max: Long): ComposableBooleanExp[Unit] =
      longConstant(long).isInInclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isInExclusiveRangeExp(min: Long, max: Long): ComposableBooleanExp[Unit] =
      longConstant(long).isInExclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isPercentageExp: ComposableBooleanExp[Unit] =
      isInInclusiveRangeExp(0, 100)
  }

  implicit class DoubleExtensions(double: Double) {

    def isGreaterThanExp(anotherDouble: Double): ComposableBooleanExp[Unit] =
      doubleConstant(double).isGreaterThan(QuantifiableExp(_ => anotherDouble))

    def isLessThanExp(anotherDouble: Double): ComposableBooleanExp[Unit] =
      doubleConstant(double).isLessThan(QuantifiableExp(_ => anotherDouble))

    def isGreaterThanOrEqualToExp(anotherDouble: Double): ComposableBooleanExp[Unit] =
      doubleConstant(double).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherDouble))

    def isLessThanOrEqualToExp(anotherDouble: Double): ComposableBooleanExp[Unit] =
      doubleConstant(double).isLessThanOrEqualTo(QuantifiableExp(_ => anotherDouble))

    def isInInclusiveRangeExp(min: Double, max: Double): ComposableBooleanExp[Unit] =
      doubleConstant(double).isInInclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isInExclusiveRangeExp(min: Double, max: Double): ComposableBooleanExp[Unit] =
      doubleConstant(double).isInExclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isPercentageExp: ComposableBooleanExp[Unit] =
      isInInclusiveRangeExp(0, 100)
  }

  implicit class BigDecimalExtensions(bigDecimal: BigDecimal) {

    def isGreaterThanExp(anotherBigDecimal: BigDecimal): ComposableBooleanExp[Unit] =
      bigDecimalConstant(bigDecimal).isGreaterThan(QuantifiableExp(_ => anotherBigDecimal))

    def isLessThanExp(anotherBigDecimal: BigDecimal): ComposableBooleanExp[Unit] =
      bigDecimalConstant(bigDecimal).isLessThan(QuantifiableExp(_ => anotherBigDecimal))

    def isGreaterThanOrEqualToExp(anotherBigDecimal: BigDecimal): ComposableBooleanExp[Unit] =
      bigDecimalConstant(bigDecimal).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherBigDecimal))

    def isLessThanOrEqualToExp(anotherBigDecimal: BigDecimal): ComposableBooleanExp[Unit] =
      bigDecimalConstant(bigDecimal).isLessThanOrEqualTo(QuantifiableExp(_ => anotherBigDecimal))

    def isInInclusiveRangeExp(min: BigDecimal, max: BigDecimal): ComposableBooleanExp[Unit] =
      bigDecimalConstant(bigDecimal).isInInclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isInExclusiveRangeExp(min: BigDecimal, max: BigDecimal): ComposableBooleanExp[Unit] =
      bigDecimalConstant(bigDecimal).isInExclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isPercentageExp: ComposableBooleanExp[Unit] =
      isInInclusiveRangeExp(BigDecimal(0), BigDecimal(100))
  }
}
