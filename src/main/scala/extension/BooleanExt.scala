package extension

import expression.{Bool, LogicalOperatorsExp}

object BooleanExt {

  import expression.BooleanExp._

  implicit class BooleanExtensions(boolean: Boolean) {

    def isTrueExp: LogicalOperatorsExp[Unit,Bool] = boolConstant(boolean).isTrue

    def isFalseExp: LogicalOperatorsExp[Unit,Bool] = boolConstant(boolean).isFalse
  }
}
