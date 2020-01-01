package expression

trait LogicalOperatorsExp[T,R <: LogicalOperators[R]] extends Expression[T,R] {

  def and(expression: LogicalOperatorsExp[T,R]): LogicalOperatorsExp[T,R] =
    AndExp[T,R](left = this, right = expression)

  def or(expression: LogicalOperatorsExp[T,R]): LogicalOperatorsExp[T,R] =
    OrExp[T,R](left = this, right = expression)
}
