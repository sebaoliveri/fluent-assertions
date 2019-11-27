package expression

trait BooleanExp[T,R <: LogicalOperators[R]] extends Expression[T,R] {

  def and(expression: BooleanExp[T,R]): BooleanExp[T,R] =
    AndExp[T,R](left = this, right = expression)

  def or(expression: BooleanExp[T,R]): BooleanExp[T,R] =
    OrExp[T,R](left = this, right = expression)
}
