package expression

case class NullBooleanExp[T,R <: LogicalOperators[R]]() extends BooleanExp[T,R] {
  override def and(expression: BooleanExp[T,R]): BooleanExp[T,R] = expression
  override def or(expression: BooleanExp[T,R]): BooleanExp[T,R] = expression
  override def evaluate(context: T): R = throw new RuntimeException("NullBooleanExp can not be evaluated")
}
