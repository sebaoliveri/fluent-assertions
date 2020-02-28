package assertion

import expression.{Bool, BoolExpBehaviour, IterableExp, NullExp, ObjectExp}

object IterableExpAssertionBuilder {
  import IterableExp._

  def fromIterableConstant[T,R](iterable: Iterable[R]): IterableExpAssertionBuilder[T,R]=
    IterableExpAssertionBuilder(iterableConstant(iterable))

  def fromIterableVariable[T,R](iterable: T => Iterable[R]): IterableExpAssertionBuilder[T,R] =
    IterableExpAssertionBuilder(iterableVariable(iterable))

  def apply[T,R](iterableExp: IterableExp[T,R]): IterableExpAssertionBuilder[T,R] =
    IterableExpAssertionBuilder(iterableExp, new NullExp[T,Bool]())

  def apply[T,R](iterableExp: IterableExp[T,R], expression: BoolExpBehaviour[T]): IterableExpAssertionBuilder[T,R] =
    new IterableExpAssertionBuilder(iterableExp, expression, _ and _)
}

case class IterableExpAssertionBuilder[T,R](iterableExp: IterableExp[T,R], expression: BoolExpBehaviour[T], operator: (BoolExpBehaviour[T], BoolExpBehaviour[T]) => BoolExpBehaviour[T])
  extends AssertionBuilder[T,IterableExpAssertionBuilder[T,R]](expression) {

  import IterableExp._

  private def indexBoundedTo(predicate: R => BoolExpBehaviour[Unit]): Int => R => BoolExpBehaviour[Unit] = index => predicate

  def forAll(predicate: R => BoolExpBehaviour[Unit]): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.forAll(indexBoundedTo(predicate)))

  def existAny(predicate: R => BoolExpBehaviour[Unit]): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.existAny(indexBoundedTo(predicate)))

  def containsAllOrdered(objects: Iterable[R]): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.containsAllInSameOrder(iterableConstant(objects)))

  def containsAllOrdered(objects: T => Iterable[R]): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.containsAllInSameOrder(iterableVariable(objects)))

  def containsAll(objects: Iterable[R]): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.containsAll(iterableConstant(objects)))

  def containsAll(objects: T => Iterable[R]): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.containsAll(iterableVariable(objects)))

  def doesNotContainAnyOf(objects: Iterable[R]): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.doesNotContainAnyOf(iterableConstant(objects)))

  def doesNotContainAnyOf(objects: T => Iterable[R]): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.doesNotContainAnyOf(iterableVariable(objects)))

  def containsAnyOf(objects: Iterable[R]): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.containsAnyOf(iterableConstant(objects)))

  def containsAnyOf(objects: T => Iterable[R]): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.containsAnyOf(iterableVariable(objects)))

  def contains(anObject: R): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.contains(ObjectExp(_ => anObject)))

  def contains(anObject: T => R): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.contains(ObjectExp(anObject)))

  def doesNotContain(anObject: R): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.doesNotContain(ObjectExp(_ => anObject)))

  def doesNotContain(anObject: T => R): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.doesNotContain(ObjectExp(anObject)))

  def containsNoDuplicates: IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.containsNoDuplicates)

  def isNotEmpty: IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.isNotEmpty)

  def isEmpty: IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.isEmpty)

  private def newWith(newExpression: BoolExpBehaviour[T]): IterableExpAssertionBuilder[T,R] =
    IterableExpAssertionBuilder(iterableExp, operator.apply(expression, newExpression))

  override def or: IterableExpAssertionBuilder[T,R] =
    IterableExpAssertionBuilder(iterableExp, expression, _ or _)
}
