package expression

case class IfTrueExp[T,R <: LogicalOperators[R]](left: LogicalOperatorsExp[T,R],
                                                 right: LogicalOperatorsExp[T,R])
                                                 extends LogicalOperatorsExp[T,R] {

  override def evaluate(context: T): R =
    left.evaluate(context).ifTrue(right.evaluate(context))
}
