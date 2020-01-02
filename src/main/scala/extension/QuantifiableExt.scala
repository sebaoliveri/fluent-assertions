package extension

import expression.{Bool, LogicalOperatorsExp, QuantifiableExp, QuantifiableOrderedExp}

object QuantifiableExt {

  import QuantifiableOrderedExp._

  implicit class IntExtensions(int: Int) {

    def isGreaterThan(anotherInt: Int): LogicalOperatorsExp[Unit,Bool] =
      intConstant(int).isGreaterThan(QuantifiableExp(_ => anotherInt))

    def isLessThan(anotherInt: Int): LogicalOperatorsExp[Unit,Bool] =
      intConstant(int).isLessThan(QuantifiableExp(_ => anotherInt))

    def isGreaterThanOrEqualTo(anotherInt: Int): LogicalOperatorsExp[Unit,Bool] =
      intConstant(int).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherInt))

    def isLessThanOrEqualTo(anotherInt: Int): LogicalOperatorsExp[Unit,Bool] =
      intConstant(int).isLessThanOrEqualTo(QuantifiableExp(_ => anotherInt))

    def isInInclusiveRange(min: Int, max: Int): LogicalOperatorsExp[Unit,Bool] =
      intConstant(int).isInInclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isInExclusiveRange(min: Int, max: Int): LogicalOperatorsExp[Unit,Bool] =
      intConstant(int).isInExclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isPercentage: LogicalOperatorsExp[Unit,Bool] =
      isInInclusiveRange(0, 100)
  }

  implicit class LongExtensions(long: Long) {

    def isGreaterThan(anotherLong: Long): LogicalOperatorsExp[Unit,Bool] =
      longConstant(long).isGreaterThan(QuantifiableExp(_ => anotherLong))

    def isLessThan(anotherLong: Long): LogicalOperatorsExp[Unit,Bool] =
      longConstant(long).isLessThan(QuantifiableExp(_ => anotherLong))

    def isGreaterThanOrEqualTo(anotherLong: Long): LogicalOperatorsExp[Unit,Bool] =
      longConstant(long).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherLong))

    def isLessThanOrEqualTo(anotherLong: Long): LogicalOperatorsExp[Unit,Bool] =
      longConstant(long).isLessThanOrEqualTo(QuantifiableExp(_ => anotherLong))

    def isInInclusiveRange(min: Long, max: Long): LogicalOperatorsExp[Unit,Bool] =
      longConstant(long).isInInclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isInExclusiveRange(min: Long, max: Long): LogicalOperatorsExp[Unit,Bool] =
      longConstant(long).isInExclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isPercentage: LogicalOperatorsExp[Unit,Bool] =
      isInInclusiveRange(0, 100)
  }

  implicit class DoubleExtensions(double: Double) {

    def isGreaterThan(anotherDouble: Double): LogicalOperatorsExp[Unit,Bool] =
      doubleConstant(double).isGreaterThan(QuantifiableExp(_ => anotherDouble))

    def isLessThan(anotherDouble: Double): LogicalOperatorsExp[Unit,Bool] =
      doubleConstant(double).isLessThan(QuantifiableExp(_ => anotherDouble))

    def isGreaterThanOrEqualTo(anotherDouble: Double): LogicalOperatorsExp[Unit,Bool] =
      doubleConstant(double).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherDouble))

    def isLessThanOrEqualTo(anotherDouble: Double): LogicalOperatorsExp[Unit,Bool] =
      doubleConstant(double).isLessThanOrEqualTo(QuantifiableExp(_ => anotherDouble))

    def isInInclusiveRange(min: Double, max: Double): LogicalOperatorsExp[Unit,Bool] =
      doubleConstant(double).isInInclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isInExclusiveRange(min: Double, max: Double): LogicalOperatorsExp[Unit,Bool] =
      doubleConstant(double).isInExclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isPercentage: LogicalOperatorsExp[Unit,Bool] =
      isInInclusiveRange(0, 100)
  }

  implicit class BigDecimalExtensions(bigDecimal: BigDecimal) {

    def isGreaterThan(anotherBigDecimal: BigDecimal): LogicalOperatorsExp[Unit,Bool] =
      bigDecimalConstant(bigDecimal).isGreaterThan(QuantifiableExp(_ => anotherBigDecimal))

    def isLessThan(anotherBigDecimal: BigDecimal): LogicalOperatorsExp[Unit,Bool] =
      bigDecimalConstant(bigDecimal).isLessThan(QuantifiableExp(_ => anotherBigDecimal))

    def isGreaterThanOrEqualTo(anotherBigDecimal: BigDecimal): LogicalOperatorsExp[Unit,Bool] =
      bigDecimalConstant(bigDecimal).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherBigDecimal))

    def isLessThanOrEqualTo(anotherBigDecimal: BigDecimal): LogicalOperatorsExp[Unit,Bool] =
      bigDecimalConstant(bigDecimal).isLessThanOrEqualTo(QuantifiableExp(_ => anotherBigDecimal))

    def isInInclusiveRange(min: BigDecimal, max: BigDecimal): LogicalOperatorsExp[Unit,Bool] =
      bigDecimalConstant(bigDecimal).isInInclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isInExclusiveRange(min: BigDecimal, max: BigDecimal): LogicalOperatorsExp[Unit,Bool] =
      bigDecimalConstant(bigDecimal).isInExclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isPercentage: LogicalOperatorsExp[Unit,Bool] =
      isInInclusiveRange(BigDecimal(0), BigDecimal(100))
  }
}
