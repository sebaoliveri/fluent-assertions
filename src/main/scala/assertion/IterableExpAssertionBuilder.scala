package assertion

import expression.{Bool, BooleanExp, IterableExp, NullBooleanExp}

object IterableExpAssertionBuilder {
  import IterableExp._

  def fromIterableConstant[T,R](iterable: Iterable[R]): IterableExpAssertionBuilder[T,R]=
    IterableExpAssertionBuilder(iterableConstant(iterable))

  def fromIterableVariable[T,R](iterable: T => Iterable[R]): IterableExpAssertionBuilder[T,R] =
    IterableExpAssertionBuilder(iterableVariable(iterable))

  def apply[T,R](iterableExp: IterableExp[T,R]): IterableExpAssertionBuilder[T,R] =
    IterableExpAssertionBuilder(iterableExp, new NullBooleanExp[T,Bool]())

  def apply[T,R](iterableExp: IterableExp[T,R], expression: BooleanExp[T,Bool]): IterableExpAssertionBuilder[T,R] =
    new IterableExpAssertionBuilder(iterableExp, expression, _ and _)
}

case class IterableExpAssertionBuilder[T,R](iterableExp: IterableExp[T,R], expression: BooleanExp[T,Bool], operator: (BooleanExp[T,Bool], BooleanExp[T,Bool]) => BooleanExp[T,Bool])
  extends BoolExpAssertionBuilder[T,IterableExpAssertionBuilder[T,R]](expression) {

  def forAll(predicate: R => Boolean): IterableExpAssertionBuilder[T, R] =
    newWith(iterableExp.forAll(predicate))

  def existAny(predicate: R => Boolean): IterableExpAssertionBuilder[T, R] =
    newWith(iterableExp.existAny(predicate))

  def isNotEmpty: IterableExpAssertionBuilder[T, R] =
    newWith(iterableExp.isNotEmpty)

  def isEmpty: IterableExpAssertionBuilder[T, R] =
    newWith(iterableExp.isEmpty)

  private def newWith(newExpression: BooleanExp[T,Bool]): IterableExpAssertionBuilder[T, R] =
    IterableExpAssertionBuilder(iterableExp, operator.apply(expression, newExpression))

  override def or: IterableExpAssertionBuilder[T, R] =
    IterableExpAssertionBuilder(iterableExp, expression, _ or _)
}
