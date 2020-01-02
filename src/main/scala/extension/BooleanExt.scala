package extension

import expression.{Bool, LogicalOperatorsExp}

object BooleanExt {

  import expression.BooleanExp._

  implicit class BooleanExtensions(boolean: Boolean) {

    def isTrue: LogicalOperatorsExp[Unit,Bool] = boolConstant(boolean).isTrue

    def isFalse: LogicalOperatorsExp[Unit,Bool] = boolConstant(boolean).isFalse
  }
}
