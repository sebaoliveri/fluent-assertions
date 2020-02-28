package extension

import expression.{BoolExpBehaviour, ObjectExp}

object IterableExt {

  import expression.IterableExp._

  implicit class IterableExtensions[R](iterable: Iterable[R]) {

    private def indexBoundedTo(predicate: R => BoolExpBehaviour[Unit]): Int => R => BoolExpBehaviour[Unit] =
      index => predicate

    def forAllExp(predicate: R => BoolExpBehaviour[Unit]): BoolExpBehaviour[Unit] =
      iterableConstant(iterable).forAll(indexBoundedTo(predicate))

    def existAnyExp(predicate: R => BoolExpBehaviour[Unit]): BoolExpBehaviour[Unit] =
      iterableConstant(iterable).existAny(indexBoundedTo(predicate))

    def containsAllOrderedExp(objects: Iterable[R]): BoolExpBehaviour[Unit] =
      iterableConstant(iterable).containsAllInSameOrder(iterableConstant(objects))

    def containsAllExp(objects: Iterable[R]): BoolExpBehaviour[Unit] =
      iterableConstant(iterable).containsAll(iterableConstant(objects))

    def doesNotContainAnyOfExp(objects: Iterable[R]): BoolExpBehaviour[Unit] =
      iterableConstant(iterable).doesNotContainAnyOf(iterableConstant(objects))

    def containsAnyOfExp(objects: Iterable[R]): BoolExpBehaviour[Unit] =
      iterableConstant(iterable).containsAnyOf(iterableConstant(objects))

    def containsExp(anObject: R): BoolExpBehaviour[Unit] =
      iterableConstant(iterable).contains(ObjectExp(_ => anObject))

    def doesNotContainExp(anObject: R): BoolExpBehaviour[Unit] =
      iterableConstant(iterable).doesNotContain(ObjectExp(_ => anObject))

    def containsNoDuplicatesExp: BoolExpBehaviour[Unit] =
      iterableConstant(iterable).containsNoDuplicates

    def isEmptyExp: BoolExpBehaviour[Unit] =
      iterableConstant(iterable).isEmpty

    def isNotEmptyExp: BoolExpBehaviour[Unit] =
      iterableConstant(iterable).isNotEmpty
  }
}
