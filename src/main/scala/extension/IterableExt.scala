package extension

import expression.{Bool, LogicalOperatorsExp, ObjectExp}

object IterableExt {

  import expression.IterableExp._

  implicit class IterableExtensions[R](iterable: Iterable[R]) {

    private def indexBoundedTo(predicate: R => LogicalOperatorsExp[Unit,Bool]): Int => R => LogicalOperatorsExp[Unit,Bool] =
      index => predicate

    def forAllExp(predicate: R => LogicalOperatorsExp[Unit,Bool]): LogicalOperatorsExp[Unit,Bool] =
      iterableConstant(iterable).forAll(indexBoundedTo(predicate))

    def existAnyExp(predicate: R => LogicalOperatorsExp[Unit,Bool]): LogicalOperatorsExp[Unit,Bool] =
      iterableConstant(iterable).existAny(indexBoundedTo(predicate))

    def containsAllOrderedExp(objects: Iterable[R]): LogicalOperatorsExp[Unit,Bool] =
      iterableConstant(iterable).containsAllInSameOrder(iterableConstant(objects))

    def containsAllExp(objects: Iterable[R]): LogicalOperatorsExp[Unit,Bool] =
      iterableConstant(iterable).containsAll(iterableConstant(objects))

    def doesNotContainAnyOfExp(objects: Iterable[R]): LogicalOperatorsExp[Unit,Bool] =
      iterableConstant(iterable).doesNotContainAnyOf(iterableConstant(objects))

    def containsAnyOfExp(objects: Iterable[R]): LogicalOperatorsExp[Unit,Bool] =
      iterableConstant(iterable).containsAnyOf(iterableConstant(objects))

    def containsExp(anObject: R): LogicalOperatorsExp[Unit,Bool] =
      iterableConstant(iterable).contains(ObjectExp(_ => anObject))

    def doesNotContainExp(anObject: R): LogicalOperatorsExp[Unit,Bool] =
      iterableConstant(iterable).doesNotContain(ObjectExp(_ => anObject))

    def containsNoDuplicatesExp: LogicalOperatorsExp[Unit,Bool] =
      iterableConstant(iterable).containsNoDuplicates

    def isEmptyExp: LogicalOperatorsExp[Unit,Bool] =
      iterableConstant(iterable).isEmpty

    def isNotEmptyExp: LogicalOperatorsExp[Unit,Bool] =
      iterableConstant(iterable).isNotEmpty
  }
}
