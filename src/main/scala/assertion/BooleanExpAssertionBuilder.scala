package assertion

import expression.{Bool, BooleanExp, LogicalOperatorsExp, NullExp}

object BooleanExpAssertionBuilder {
  import BooleanExp._

  def fromBooleanConstant[T](boolean: Boolean): BooleanExpAssertionBuilder[T]=
    BooleanExpAssertionBuilder(boolConstant(boolean))

  def fromBooleanVariable[T](boolean: T => Boolean): BooleanExpAssertionBuilder[T]=
    BooleanExpAssertionBuilder(boolVariable(boolean))

  def apply[T](boolExp: BooleanExp[T]): BooleanExpAssertionBuilder[T] =
    BooleanExpAssertionBuilder(boolExp, new NullExp[T,Bool]())

  def apply[T](boolExp: BooleanExp[T], expression: LogicalOperatorsExp[T,Bool]): BooleanExpAssertionBuilder[T] =
    new BooleanExpAssertionBuilder(boolExp, expression, _ and _)
}

case class BooleanExpAssertionBuilder[T](boolExp: BooleanExp[T],
                                         expression: LogicalOperatorsExp[T,Bool],
                                         operator: (LogicalOperatorsExp[T,Bool], LogicalOperatorsExp[T,Bool]) => LogicalOperatorsExp[T,Bool])
  extends AssertionBuilder[T,BooleanExpAssertionBuilder[T]](expression) {

  def isTrue: BooleanExpAssertionBuilder[T] =
    BooleanExpAssertionBuilder(boolExp, operator.apply(expression, boolExp.isTrue))

  def isFalse: BooleanExpAssertionBuilder[T] =
    BooleanExpAssertionBuilder(boolExp, operator.apply(expression, boolExp.isFalse))

  override def or: BooleanExpAssertionBuilder[T] = BooleanExpAssertionBuilder(boolExp, expression, _ or _)
}
