package expression

case class NullExp[T,R <: LogicalOperators[R]]() extends LogicalOperatorsExp[T,Bool] {

  override def and(expression: LogicalOperatorsExp[T,Bool]): LogicalOperatorsExp[T,Bool] =
    expression

  override def or(expression: LogicalOperatorsExp[T,Bool]): LogicalOperatorsExp[T,Bool] =
    expression

  override def evaluate(context: T): Bool = ??? // do nothing
}
