package assertion

import expression._

object ObjectExpAssertionBuilder {
  def fromObjectConstant[T,R](anObject: T): ObjectExpAssertionBuilder[T,R]= ObjectExpAssertionBuilder(ObjectExp(_ => anObject.asInstanceOf[R]))
  def fromObjectVariable[T,R](anObject: T => R): ObjectExpAssertionBuilder[T,R] = ObjectExpAssertionBuilder(ObjectExp(anObject))
  def apply[T,R](objectExp: ObjectExp[T,R]): ObjectExpAssertionBuilder[T,R] = new ObjectExpAssertionBuilder(objectExp, new NullBooleanExp[T,Bool](), _ and _)
}

case class ObjectExpAssertionBuilder[T,R](objectExp: ObjectExp[T,R], expression: BooleanExp[T,Bool], operator: (BooleanExp[T,Bool], BooleanExp[T,Bool]) => BooleanExp[T,Bool]) extends AssertionBuilder[T] {
  private def newBoolExp(anExpression: BooleanExp[T, Bool]): BoolExpAssertionBuilder[T, ObjectExpAssertionBuilder[T,R]] =
    BoolExpAssertionBuilder(operator(expression, anExpression), (newExpression, newOperator) => ObjectExpAssertionBuilder(objectExp, newExpression, newOperator))
  def isEqualTo(any: R): BoolExpAssertionBuilder[T, ObjectExpAssertionBuilder[T,R]] = isEqualTo({_:T => any})
  def isEqualTo(any: T => R): BoolExpAssertionBuilder[T,ObjectExpAssertionBuilder[T,R]] = newBoolExp(objectExp.isEqualTo(ObjectExp(any)))
  def isTrueThat(predicate: R => Boolean): BoolExpAssertionBuilder[T, ObjectExpAssertionBuilder[T,R]] = newBoolExp(objectExp.isTrue(predicate))
  def isFalseThat(predicate: R => Boolean): BoolExpAssertionBuilder[T, ObjectExpAssertionBuilder[T,R]] = newBoolExp(NotExp(objectExp.isTrue(predicate)))
}
