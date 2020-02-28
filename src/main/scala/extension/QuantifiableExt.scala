package extension

import expression.{Bool, BoolExpBehaviour, LogicalOperatorsExp, QuantifiableExp, QuantifiableOrderedExp}

object QuantifiableExt {

  import QuantifiableOrderedExp._

  implicit class IntExtensions(int: Int) {

    def isGreaterThanExp(anotherInt: Int): BoolExpBehaviour[Unit] =
      intConstant(int).isGreaterThan(QuantifiableExp(_ => anotherInt))

    def isLessThanExp(anotherInt: Int): BoolExpBehaviour[Unit] =
      intConstant(int).isLessThan(QuantifiableExp(_ => anotherInt))

    def isGreaterThanOrEqualToExp(anotherInt: Int): BoolExpBehaviour[Unit] =
      intConstant(int).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherInt))

    def isLessThanOrEqualToExp(anotherInt: Int): BoolExpBehaviour[Unit] =
      intConstant(int).isLessThanOrEqualTo(QuantifiableExp(_ => anotherInt))

    def isInInclusiveRangeExp(min: Int, max: Int): BoolExpBehaviour[Unit] =
      intConstant(int).isInInclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isInExclusiveRangeExp(min: Int, max: Int): BoolExpBehaviour[Unit] =
      intConstant(int).isInExclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isPercentageExp: BoolExpBehaviour[Unit] =
      isInInclusiveRangeExp(0, 100)
  }

  implicit class LongExtensions(long: Long) {

    def isGreaterThanExp(anotherLong: Long): BoolExpBehaviour[Unit] =
      longConstant(long).isGreaterThan(QuantifiableExp(_ => anotherLong))

    def isLessThanExp(anotherLong: Long): BoolExpBehaviour[Unit] =
      longConstant(long).isLessThan(QuantifiableExp(_ => anotherLong))

    def isGreaterThanOrEqualToExp(anotherLong: Long): BoolExpBehaviour[Unit] =
      longConstant(long).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherLong))

    def isLessThanOrEqualToExp(anotherLong: Long): BoolExpBehaviour[Unit] =
      longConstant(long).isLessThanOrEqualTo(QuantifiableExp(_ => anotherLong))

    def isInInclusiveRangeExp(min: Long, max: Long): BoolExpBehaviour[Unit] =
      longConstant(long).isInInclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isInExclusiveRangeExp(min: Long, max: Long): BoolExpBehaviour[Unit] =
      longConstant(long).isInExclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isPercentageExp: BoolExpBehaviour[Unit] =
      isInInclusiveRangeExp(0, 100)
  }

  implicit class DoubleExtensions(double: Double) {

    def isGreaterThanExp(anotherDouble: Double): BoolExpBehaviour[Unit] =
      doubleConstant(double).isGreaterThan(QuantifiableExp(_ => anotherDouble))

    def isLessThanExp(anotherDouble: Double): BoolExpBehaviour[Unit] =
      doubleConstant(double).isLessThan(QuantifiableExp(_ => anotherDouble))

    def isGreaterThanOrEqualToExp(anotherDouble: Double): BoolExpBehaviour[Unit] =
      doubleConstant(double).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherDouble))

    def isLessThanOrEqualToExp(anotherDouble: Double): BoolExpBehaviour[Unit] =
      doubleConstant(double).isLessThanOrEqualTo(QuantifiableExp(_ => anotherDouble))

    def isInInclusiveRangeExp(min: Double, max: Double): BoolExpBehaviour[Unit] =
      doubleConstant(double).isInInclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isInExclusiveRangeExp(min: Double, max: Double): BoolExpBehaviour[Unit] =
      doubleConstant(double).isInExclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isPercentageExp: BoolExpBehaviour[Unit] =
      isInInclusiveRangeExp(0, 100)
  }

  implicit class BigDecimalExtensions(bigDecimal: BigDecimal) {

    def isGreaterThanExp(anotherBigDecimal: BigDecimal): BoolExpBehaviour[Unit] =
      bigDecimalConstant(bigDecimal).isGreaterThan(QuantifiableExp(_ => anotherBigDecimal))

    def isLessThanExp(anotherBigDecimal: BigDecimal): BoolExpBehaviour[Unit] =
      bigDecimalConstant(bigDecimal).isLessThan(QuantifiableExp(_ => anotherBigDecimal))

    def isGreaterThanOrEqualToExp(anotherBigDecimal: BigDecimal): BoolExpBehaviour[Unit] =
      bigDecimalConstant(bigDecimal).isGreaterThanOrEqualTo(QuantifiableExp(_ => anotherBigDecimal))

    def isLessThanOrEqualToExp(anotherBigDecimal: BigDecimal): BoolExpBehaviour[Unit] =
      bigDecimalConstant(bigDecimal).isLessThanOrEqualTo(QuantifiableExp(_ => anotherBigDecimal))

    def isInInclusiveRangeExp(min: BigDecimal, max: BigDecimal): BoolExpBehaviour[Unit] =
      bigDecimalConstant(bigDecimal).isInInclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isInExclusiveRangeExp(min: BigDecimal, max: BigDecimal): BoolExpBehaviour[Unit] =
      bigDecimalConstant(bigDecimal).isInExclusiveRange(QuantifiableExp(_ => min), QuantifiableExp(_ => max))

    def isPercentageExp: BoolExpBehaviour[Unit] =
      isInInclusiveRangeExp(BigDecimal(0), BigDecimal(100))
  }
}
