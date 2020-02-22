package extension

import expression.{Bool, LogicalOperatorsExp, QuantifiableExp, QuantifiableOrderedExp}

object QuantifiableExt {

  import QuantifiableOrderedExp._

  implicit class IntExtensions(int: Int) {

    def isGreaterThanExp(anotherInt: Int): LogicalOperatorsExp[Unit,Bool] =
      intConstant(int).isGreaterThan(QuantifiableExp(_ => anotherInt))

    def isLessThanExp(anotherInt: Int): LogicalOperatorsExp[Unit,Bool] =
      intConstant(int).isLessThan(QuantifiableExp(_ => anotherInt))

    def isGreaterThanOrEqualToExp(anotherInt: Int): LogicalOperatorsExp[Unit,Bool] =
      intConstant(int).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherInt))

    def isLessThanOrEqualToExp(anotherInt: Int): LogicalOperatorsExp[Unit,Bool] =
      intConstant(int).isLessThanOrEqualTo(QuantifiableExp(_ => anotherInt))

    def isInInclusiveRangeExp(min: Int, max: Int): LogicalOperatorsExp[Unit,Bool] =
      intConstant(int).isInInclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isInExclusiveRangeExp(min: Int, max: Int): LogicalOperatorsExp[Unit,Bool] =
      intConstant(int).isInExclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isPercentageExp: LogicalOperatorsExp[Unit,Bool] =
      isInInclusiveRangeExp(0, 100)
  }

  implicit class LongExtensions(long: Long) {

    def isGreaterThanExp(anotherLong: Long): LogicalOperatorsExp[Unit,Bool] =
      longConstant(long).isGreaterThan(QuantifiableExp(_ => anotherLong))

    def isLessThanExp(anotherLong: Long): LogicalOperatorsExp[Unit,Bool] =
      longConstant(long).isLessThan(QuantifiableExp(_ => anotherLong))

    def isGreaterThanOrEqualToExp(anotherLong: Long): LogicalOperatorsExp[Unit,Bool] =
      longConstant(long).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherLong))

    def isLessThanOrEqualToExp(anotherLong: Long): LogicalOperatorsExp[Unit,Bool] =
      longConstant(long).isLessThanOrEqualTo(QuantifiableExp(_ => anotherLong))

    def isInInclusiveRangeExp(min: Long, max: Long): LogicalOperatorsExp[Unit,Bool] =
      longConstant(long).isInInclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isInExclusiveRangeExp(min: Long, max: Long): LogicalOperatorsExp[Unit,Bool] =
      longConstant(long).isInExclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isPercentageExp: LogicalOperatorsExp[Unit,Bool] =
      isInInclusiveRangeExp(0, 100)
  }

  implicit class DoubleExtensions(double: Double) {

    def isGreaterThanExp(anotherDouble: Double): LogicalOperatorsExp[Unit,Bool] =
      doubleConstant(double).isGreaterThan(QuantifiableExp(_ => anotherDouble))

    def isLessThanExp(anotherDouble: Double): LogicalOperatorsExp[Unit,Bool] =
      doubleConstant(double).isLessThan(QuantifiableExp(_ => anotherDouble))

    def isGreaterThanOrEqualToExp(anotherDouble: Double): LogicalOperatorsExp[Unit,Bool] =
      doubleConstant(double).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherDouble))

    def isLessThanOrEqualToExp(anotherDouble: Double): LogicalOperatorsExp[Unit,Bool] =
      doubleConstant(double).isLessThanOrEqualTo(QuantifiableExp(_ => anotherDouble))

    def isInInclusiveRangeExp(min: Double, max: Double): LogicalOperatorsExp[Unit,Bool] =
      doubleConstant(double).isInInclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isInExclusiveRangeExp(min: Double, max: Double): LogicalOperatorsExp[Unit,Bool] =
      doubleConstant(double).isInExclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isPercentageExp: LogicalOperatorsExp[Unit,Bool] =
      isInInclusiveRangeExp(0, 100)
  }

  implicit class BigDecimalExtensions(bigDecimal: BigDecimal) {

    def isGreaterThanExp(anotherBigDecimal: BigDecimal): LogicalOperatorsExp[Unit,Bool] =
      bigDecimalConstant(bigDecimal).isGreaterThan(QuantifiableExp(_ => anotherBigDecimal))

    def isLessThanExp(anotherBigDecimal: BigDecimal): LogicalOperatorsExp[Unit,Bool] =
      bigDecimalConstant(bigDecimal).isLessThan(QuantifiableExp(_ => anotherBigDecimal))

    def isGreaterThanOrEqualToExp(anotherBigDecimal: BigDecimal): LogicalOperatorsExp[Unit,Bool] =
      bigDecimalConstant(bigDecimal).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherBigDecimal))

    def isLessThanOrEqualToExp(anotherBigDecimal: BigDecimal): LogicalOperatorsExp[Unit,Bool] =
      bigDecimalConstant(bigDecimal).isLessThanOrEqualTo(QuantifiableExp(_ => anotherBigDecimal))

    def isInInclusiveRangeExp(min: BigDecimal, max: BigDecimal): LogicalOperatorsExp[Unit,Bool] =
      bigDecimalConstant(bigDecimal).isInInclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isInExclusiveRangeExp(min: BigDecimal, max: BigDecimal): LogicalOperatorsExp[Unit,Bool] =
      bigDecimalConstant(bigDecimal).isInExclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isPercentageExp: LogicalOperatorsExp[Unit,Bool] =
      isInInclusiveRangeExp(BigDecimal(0), BigDecimal(100))
  }
}
