package org.nulluncertainty.extension

import org.nulluncertainty.expression.{ComposableBooleanExp, ObjectExp}

object IterableExt {

  import org.nulluncertainty.expression.IterableExp._

  implicit class IterableExtensions[R](iterable: Iterable[R]) {

    private def indexBoundedTo(predicate: R => ComposableBooleanExp[Unit]): Int => R => ComposableBooleanExp[Unit] =
      index => predicate

    def forAllExp(predicate: R => ComposableBooleanExp[Unit]): ComposableBooleanExp[Unit] =
      iterableConstant(iterable).forAll(indexBoundedTo(predicate))

    def existAnyExp(predicate: R => ComposableBooleanExp[Unit]): ComposableBooleanExp[Unit] =
      iterableConstant(iterable).existAny(indexBoundedTo(predicate))

    def containsAllOrderedExp(objects: Iterable[R]): ComposableBooleanExp[Unit] =
      iterableConstant(iterable).containsAllInSameOrder(iterableConstant(objects))

    def containsAllExp(objects: Iterable[R]): ComposableBooleanExp[Unit] =
      iterableConstant(iterable).containsAll(iterableConstant(objects))

    def doesNotContainAnyOfExp(objects: Iterable[R]): ComposableBooleanExp[Unit] =
      iterableConstant(iterable).doesNotContainAnyOf(iterableConstant(objects))

    def containsAnyOfExp(objects: Iterable[R]): ComposableBooleanExp[Unit] =
      iterableConstant(iterable).containsAnyOf(iterableConstant(objects))

    def containsExp(anObject: R): ComposableBooleanExp[Unit] =
      iterableConstant(iterable).contains(ObjectExp(_ => anObject))

    def doesNotContainExp(anObject: R): ComposableBooleanExp[Unit] =
      iterableConstant(iterable).doesNotContain(ObjectExp(_ => anObject))

    def containsNoDuplicatesExp: ComposableBooleanExp[Unit] =
      iterableConstant(iterable).containsNoDuplicates

    def isEmptyExp: ComposableBooleanExp[Unit] =
      iterableConstant(iterable).isEmpty

    def isNotEmptyExp: ComposableBooleanExp[Unit] =
      iterableConstant(iterable).isNotEmpty
  }
}
