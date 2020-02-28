package assertion

import expression.{Bool, BoolExpBehaviour, IsDefinedExp, IterableExp, NullExp, ObjectExp, OptionalBoolExp, OptionalExp}

object OptionalIterableExpAssertionBuilder {

  import OptionalExp._

  def fromMaybeIterableConstant[T,R](maybeIterable: Option[Iterable[R]]): OptionalIterableExpAssertionBuilder[T,R] =
    OptionalIterableExpAssertionBuilder(maybeOptionConstant(maybeIterable))

  def fromMaybeIterableVariable[T,R](maybeIterable: T => Option[Iterable[R]]): OptionalIterableExpAssertionBuilder[T,R] =
    OptionalIterableExpAssertionBuilder(maybeOptionVariable(maybeIterable))

  def apply[T,R](optionalExp: OptionalExp[T,Iterable[R]]): OptionalIterableExpAssertionBuilder[T,R] =
    OptionalIterableExpAssertionBuilder(optionalExp, new NullExp[T,Bool]())

  def apply[T,R](optionalExp: OptionalExp[T,Iterable[R]], expression: BoolExpBehaviour[T]): OptionalIterableExpAssertionBuilder[T,R] =
    new OptionalIterableExpAssertionBuilder(optionalExp, expression, _ and _)
}

case class OptionalIterableExpAssertionBuilder[T,R](optionExp: OptionalExp[T,Iterable[R]], expression: BoolExpBehaviour[T], operator: (BoolExpBehaviour[T], BoolExpBehaviour[T]) => BoolExpBehaviour[T])
  extends AssertionBuilder[T,OptionalIterableExpAssertionBuilder[T,R]](expression) {

  import IterableExp._

  def isDefined: OptionalIterableExpAssertionBuilder[T,R] =
    OptionalIterableExpAssertionBuilder(optionExp, operator(expression, IsDefinedExp[T,Iterable[R]](optionExp)))

  private def indexBoundedTo(predicate: R => BoolExpBehaviour[Unit]): Int => R => BoolExpBehaviour[Unit] = index => predicate

  def forAll(predicate: R => BoolExpBehaviour[Unit]): OptionalIterableExpAssertionBuilder[T,R] =
    newWith(iterableConstant(_).forAll(indexBoundedTo(predicate)))

  def existAny(predicate: R => BoolExpBehaviour[Unit]): OptionalIterableExpAssertionBuilder[T,R] =
    newWith(iterableConstant(_).existAny(indexBoundedTo(predicate)))

  def containsAllOrdered(objects: Iterable[R]): OptionalIterableExpAssertionBuilder[T,R] =
    newWith(iterableConstant(_).containsAllInSameOrder(iterableConstant(objects)))

  def containsAllOrdered(objects: T => Iterable[R]): OptionalIterableExpAssertionBuilder[T,R] =
    newWith(iterableConstant(_).containsAllInSameOrder(iterableVariable(objects)))

  def containsAll(objects: Iterable[R]): OptionalIterableExpAssertionBuilder[T,R] =
    newWith(iterableConstant(_).containsAll(iterableConstant(objects)))

  def containsAll(objects: T => Iterable[R]): OptionalIterableExpAssertionBuilder[T,R] =
    newWith(iterableConstant(_).containsAll(iterableVariable(objects)))

  def doesNotContainAnyOf(objects: Iterable[R]): OptionalIterableExpAssertionBuilder[T,R] =
    newWith(iterableConstant(_).doesNotContainAnyOf(iterableConstant(objects)))

  def doesNotContainAnyOf(objects: T => Iterable[R]): OptionalIterableExpAssertionBuilder[T,R] =
    newWith(iterableConstant(_).doesNotContainAnyOf(iterableVariable(objects)))

  def containsAnyOf(objects: Iterable[R]): OptionalIterableExpAssertionBuilder[T,R] =
    newWith(iterableConstant(_).containsAnyOf(iterableConstant(objects)))

  def containsAnyOf(objects: T => Iterable[R]): OptionalIterableExpAssertionBuilder[T,R] =
    newWith(iterableConstant(_).containsAnyOf(iterableVariable(objects)))

  def contains(anObject: R): OptionalIterableExpAssertionBuilder[T,R] =
    newWith(iterableConstant(_).contains(ObjectExp(_ => anObject)))

  def contains(anObject: T => R): OptionalIterableExpAssertionBuilder[T,R] =
    newWith(iterableConstant(_).contains(ObjectExp(anObject)))

  def doesNotContain(anObject: R): OptionalIterableExpAssertionBuilder[T,R] =
    newWith(iterableConstant(_).doesNotContain(ObjectExp(_ => anObject)))

  def doesNotContain(anObject: T => R): OptionalIterableExpAssertionBuilder[T,R] =
    newWith(iterableConstant(_).doesNotContain(ObjectExp(anObject)))

  def containsNoDuplicates: OptionalIterableExpAssertionBuilder[T,R] =
    newWith(iterableConstant(_).containsNoDuplicates)

  def isNotEmpty: OptionalIterableExpAssertionBuilder[T,R] =
    newWith(iterableConstant(_).isNotEmpty)

  def isEmpty: OptionalIterableExpAssertionBuilder[T,R] =
    newWith(iterableConstant(_).isEmpty)

  override def or: OptionalIterableExpAssertionBuilder[T, R] =
    OptionalIterableExpAssertionBuilder(optionExp, expression, _ or _)

  private def newWith(newExpression: Iterable[R] => BoolExpBehaviour[T]): OptionalIterableExpAssertionBuilder[T,R] =
    OptionalIterableExpAssertionBuilder(optionExp, operator.apply(expression, OptionalBoolExp[T,Iterable[R]](optionExp, newExpression)))
}
