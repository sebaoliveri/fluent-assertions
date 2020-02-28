package expression

trait BoolExpBehaviour[T] extends LogicalOperatorsExp[T,Bool] {

  def and(expression: BoolExpBehaviour[T]): BoolExpBehaviour[T] =
    new AndExp(left = this, right = expression) with BoolExpBehaviour[T]

  def or(expression: BoolExpBehaviour[T]): BoolExpBehaviour[T] =
    new OrExp(left = this, right = expression) with BoolExpBehaviour[T]

  def ifTrue(expression: BoolExpBehaviour[T]): BoolExpBehaviour[T] =
    new IfTrueExp(left = this, right = expression) with BoolExpBehaviour[T]

  def ifFalse(expression: BoolExpBehaviour[T]): BoolExpBehaviour[T] =
    new IfFalseExp(left = this, right = expression) with BoolExpBehaviour[T]

  def thenElse(anAssertion: LogicalOperatorsExp[T,AssertionResultBehaviour[T]],
               anotherAssertion: LogicalOperatorsExp[T,AssertionResultBehaviour[T]]): ConditionalAssertionExp[T] =
    ConditionalAssertionExp(this, anAssertion, anotherAssertion)
}
