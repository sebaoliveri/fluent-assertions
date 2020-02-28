package expression

case class NullExp[T,R <: LogicalOperators[R]]() extends BoolExpBehaviour[T] {

  override def and(expression: BoolExpBehaviour[T]): BoolExpBehaviour[T] =
    expression

  override def or(expression: BoolExpBehaviour[T]): BoolExpBehaviour[T] =
    expression

  override def evaluate(context: T): Bool = ??? // do nothing
}
