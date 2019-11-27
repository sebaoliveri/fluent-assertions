package expression

case class NullBooleanExp[T,R <: LogicalOperators[R]]() extends BooleanExp[T,Bool] {

  override def and(expression: BooleanExp[T,Bool]): BooleanExp[T,Bool] =
    expression

  override def or(expression: BooleanExp[T,Bool]): BooleanExp[T,Bool] =
    expression

  override def evaluate(context: T): Bool =
    TrueExp
}
