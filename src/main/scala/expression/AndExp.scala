package expression

case class AndExp[T,R <: LogicalOperators[R]](left: Expression[T,R], right: Expression[T,R])
  extends LogicalOperatorsExp[T,R] {

  override def evaluate(context: T): R =
    left.evaluate(context).and(right.evaluate(context))
}
