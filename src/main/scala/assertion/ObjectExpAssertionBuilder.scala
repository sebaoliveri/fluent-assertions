package assertion

import expression._

object ObjectExpAssertionBuilder {

  def fromObjectConstant[T,R](anObject: T): ObjectExpAssertionBuilder[T,R]=
    ObjectExpAssertionBuilder(ObjectExp(_ => anObject.asInstanceOf[R]))

  def fromObjectVariable[T,R](anObject: T => R): ObjectExpAssertionBuilder[T,R] =
    ObjectExpAssertionBuilder(ObjectExp(anObject))

  def apply[T,R](objectExp: ObjectExp[T,R]): ObjectExpAssertionBuilder[T,R] =
    ObjectExpAssertionBuilder(objectExp, new NullBooleanExp[T,Bool]())

  def apply[T,R](objectExp: ObjectExp[T,R], expression: BooleanExp[T,Bool]): ObjectExpAssertionBuilder[T,R] =
    new ObjectExpAssertionBuilder(objectExp, expression, _ and _)
}

case class ObjectExpAssertionBuilder[T,R](objectExp: ObjectExp[T,R], expression: BooleanExp[T,Bool], operator: (BooleanExp[T,Bool], BooleanExp[T,Bool]) => BooleanExp[T,Bool])
  extends BoolExpAssertionBuilder[T,ObjectExpAssertionBuilder[T,R]](expression) {

  def isEqualTo(any: R): BoolExpAssertionBuilder[T, ObjectExpAssertionBuilder[T,R]] =
    isEqualTo({_:T => any})

  def isEqualTo(any: T => R): BoolExpAssertionBuilder[T,ObjectExpAssertionBuilder[T,R]] =
    newWith(objectExp.isEqualTo(ObjectExp(any)))

  def isTrueThat(predicate: R => Boolean): BoolExpAssertionBuilder[T, ObjectExpAssertionBuilder[T,R]] =
    newWith(objectExp.isTrue(predicate))

  def isFalseThat(predicate: R => Boolean): BoolExpAssertionBuilder[T, ObjectExpAssertionBuilder[T,R]] =
    newWith(NotExp(objectExp.isTrue(predicate)))

  override def or: ObjectExpAssertionBuilder[T, R] =
    ObjectExpAssertionBuilder(objectExp, expression, _ or _)

  private def newWith(newExpression: BooleanExp[T,Bool]) =
    ObjectExpAssertionBuilder(objectExp, operator.apply(expression, newExpression))
}
