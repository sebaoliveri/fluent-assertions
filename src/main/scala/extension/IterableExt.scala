package extension

import expression.{Bool, LogicalOperatorsExp}

object IterableExt {

  import expression.IterableExp._

  implicit class IterableExtensions[R](iterable: Iterable[R]) {

    def containsNoDuplicatesExp: LogicalOperatorsExp[Unit,Bool] =
      iterableConstant(iterable).containsNoDuplicates

    def isEmptyExp: LogicalOperatorsExp[Unit,Bool] =
      iterableConstant(iterable).isEmpty

    def isNotEmptyExp: LogicalOperatorsExp[Unit,Bool] =
      iterableConstant(iterable).isNotEmpty

    //TODO add other extensions
  }
}
