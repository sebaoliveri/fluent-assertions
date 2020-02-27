package expression

case class ConditionalAssertionExp[T](predicate: LogicalOperatorsExp[T,Bool],
                                      trueExp: LogicalOperatorsExp[T,AssertionResultBehaviour[T]],
                                      falseExp: LogicalOperatorsExp[T,AssertionResultBehaviour[T]])
                                      extends AssertionExpBehaviour[T,T,T] {

  override def evaluate(context: T): AssertionResultBehaviour[T] =
    predicate.evaluate(context)
      .thenElse(
        trueExp.evaluate(context),
        falseExp.evaluate(context))
}
