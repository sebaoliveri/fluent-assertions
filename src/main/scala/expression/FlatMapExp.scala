package expression

case class FlatMapExp[T](left: LogicalOperatorsExp[T,AssertionResultBehaviour[T]],
                         right: LogicalOperatorsExp[T,AssertionResultBehaviour[T]])
  extends LogicalOperatorsExp[T,AssertionResultBehaviour[T]] {

  def followedBy(assertionExp: AssertionExp[T]): FlatMapExp[T] =
    FlatMapExp(left = this, right = assertionExp)

  override def evaluate(context: T): AssertionResultBehaviour[T] =
    left.evaluate(context).flatMap(_ => right.evaluate(context))
}
