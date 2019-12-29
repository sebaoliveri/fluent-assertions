package assertion

import expression.{Bool, BooleanExp, IterableExp, NullBooleanExp, ObjectExp}

object IterableExpAssertionBuilder {
  import IterableExp._

  def fromIterableConstant[T,R](iterable: collection.immutable.Iterable[R]): IterableExpAssertionBuilder[T,R]=
    IterableExpAssertionBuilder(iterableConstant(iterable))

  def fromIterableVariable[T,R](iterable: T => collection.immutable.Iterable[R]): IterableExpAssertionBuilder[T,R] =
    IterableExpAssertionBuilder(iterableVariable(iterable))

  def apply[T,R](iterableExp: IterableExp[T,R]): IterableExpAssertionBuilder[T,R] =
    IterableExpAssertionBuilder(iterableExp, new NullBooleanExp[T,Bool]())

  def apply[T,R](iterableExp: IterableExp[T,R], expression: BooleanExp[T,Bool]): IterableExpAssertionBuilder[T,R] =
    new IterableExpAssertionBuilder(iterableExp, expression, _ and _)
}

case class IterableExpAssertionBuilder[T,R](iterableExp: IterableExp[T,R], expression: BooleanExp[T,Bool], operator: (BooleanExp[T,Bool], BooleanExp[T,Bool]) => BooleanExp[T,Bool])
  extends BoolExpAssertionBuilder[T,IterableExpAssertionBuilder[T,R]](expression) {

  import IterableExp._

  def forAll(predicate: (R,Int) => Boolean): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.forAll(predicate))

  def existAny(predicate: (R,Int) => Boolean): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.existAny(predicate))

  def containsAllOrdered(objects: collection.immutable.Iterable[R]): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.containsAllInSameOrder(iterableConstant(objects)))

  def containsAllOrdered(objects: T => collection.immutable.Iterable[R]): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.containsAllInSameOrder(iterableVariable(objects)))

  def containsAll(objects: collection.immutable.Iterable[R]): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.containsAll(iterableConstant(objects)))

  def containsAll(objects: T => collection.immutable.Iterable[R]): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.containsAll(iterableVariable(objects)))

  def doesNotContainAnyOf(objects: collection.immutable.Iterable[R]): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.doesNotContainAnyOf(iterableConstant(objects)))

  def doesNotContainAnyOf(objects: T => collection.immutable.Iterable[R]): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.doesNotContainAnyOf(iterableVariable(objects)))

  def containsAnyOf(objects: collection.immutable.Iterable[R]): IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.containsAnyOf(iterableConstant(objects)))

  def containsAnyOf(objects: T => collection.immutable.Iterable[R]): IterableExpAssertionBuilder[T,R] =
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

  def containsNoDuplicatesMatching[R1](criteria: R => R1): IterableExpAssertionBuilder[T, R] =
    newWith(iterableExp.containsNoDuplicatesMatching(criteria))

  def isNotEmpty: IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.isNotEmpty)

  def isEmpty: IterableExpAssertionBuilder[T,R] =
    newWith(iterableExp.isEmpty)

  private def newWith(newExpression: BooleanExp[T,Bool]): IterableExpAssertionBuilder[T,R] =
    IterableExpAssertionBuilder(iterableExp, operator.apply(expression, newExpression))

  override def or: IterableExpAssertionBuilder[T,R] =
    IterableExpAssertionBuilder(iterableExp, expression, _ or _)
}
