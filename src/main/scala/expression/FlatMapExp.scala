package expression

case class FlatMapExp[T](left: AssertionExp[T], right: AssertionExp[T])
  extends LogicalOperatorsExp[T,AssertionResultBehaviour[T]] {

  override def evaluate(context: T): AssertionResultBehaviour[T] =
    left.evaluate(context).flatMap(_ => right.evaluate(context))
}
