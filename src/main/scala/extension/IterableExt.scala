package extension

import expression.{Bool, LogicalOperatorsExp}

object IterableExt {

  import expression.IterableExp._

  implicit class IterableExtensions[R](iterable: Iterable[R]) {

    def containsNoDuplicates: LogicalOperatorsExp[Unit,Bool] =
      iterableConstant(iterable).containsNoDuplicates

    //TODO add other extensions
  }
}
