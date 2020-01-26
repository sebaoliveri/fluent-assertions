package expression

trait LogicalOperatorsExp[T,R <: LogicalOperators[R]] extends Expression[T,R] {

  def and(expression: LogicalOperatorsExp[T,R]): LogicalOperatorsExp[T,R] =
    AndExp(left = this, right = expression)

  def or(expression: LogicalOperatorsExp[T,R]): LogicalOperatorsExp[T,R] =
    OrExp(left = this, right = expression)

  def ifTrue(expression: LogicalOperatorsExp[T,R]): LogicalOperatorsExp[T,R] =
    IfTrueExp(left = this, right = expression)
}
