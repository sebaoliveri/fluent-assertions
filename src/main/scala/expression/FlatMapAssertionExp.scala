package expression

case class FlatMapAssertionExp[T](left: LogicalOperatorsExp[T,AssertionResultBehaviour[T]],
                                  right: LogicalOperatorsExp[T,AssertionResultBehaviour[T]])
                                  extends FollowedBy[T] {

  override def evaluate(context: T): AssertionResultBehaviour[T] =
    left.evaluate(context).flatMap(_ => right.evaluate(context))
}

trait FollowedBy[T] extends LogicalOperatorsExp[T,AssertionResultBehaviour[T]] {

  def followedBy(assertionExp: LogicalOperatorsExp[T,AssertionResultBehaviour[T]]): FlatMapAssertionExp[T] =
    FlatMapAssertionExp(left = this, right = assertionExp)
}