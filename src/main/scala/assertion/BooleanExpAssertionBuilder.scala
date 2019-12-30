package assertion

import expression.{Bool, BoolExp, BooleanExp, NullBooleanExp}

object BooleanExpAssertionBuilder {
  import BoolExp._

  def fromBooleanConstant[T](boolean: Boolean): BooleanExpAssertionBuilder[T]=
    BooleanExpAssertionBuilder(boolConstant(boolean))

  def fromBooleanVariable[T](boolean: T => Boolean): BooleanExpAssertionBuilder[T]=
    BooleanExpAssertionBuilder(boolVariable(boolean))

  def apply[T](boolExp: BoolExp[T]): BooleanExpAssertionBuilder[T] =
    BooleanExpAssertionBuilder(boolExp, new NullBooleanExp[T,Bool]())

  def apply[T](boolExp: BoolExp[T], expression: BooleanExp[T,Bool]): BooleanExpAssertionBuilder[T] =
    new BooleanExpAssertionBuilder(boolExp, expression, _ and _)
}

case class BooleanExpAssertionBuilder[T](boolExp: BoolExp[T], expression: BooleanExp[T,Bool], operator: (BooleanExp[T,Bool], BooleanExp[T,Bool]) => BooleanExp[T,Bool])
  extends BoolExpAssertionBuilder[T,BooleanExpAssertionBuilder[T]](expression) {

  def isTrue: BooleanExpAssertionBuilder[T] =
    BooleanExpAssertionBuilder(boolExp, operator.apply(expression, boolExp.isTrue))

  def isFalse: BooleanExpAssertionBuilder[T] =
    BooleanExpAssertionBuilder(boolExp, operator.apply(expression, boolExp.isFalse))

  override def or: BooleanExpAssertionBuilder[T] = BooleanExpAssertionBuilder(boolExp, expression, _ or _)
}
