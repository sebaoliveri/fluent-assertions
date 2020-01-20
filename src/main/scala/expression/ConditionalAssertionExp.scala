package expression

case class ConditionalAssertionExp[T](predicate: T => Boolean,
                                      trueExp: LogicalOperatorsExp[T,AssertionResultBehaviour[T]],
                                      falseExp: LogicalOperatorsExp[T,AssertionResultBehaviour[T]])
                                      extends LogicalOperatorsExp[T,AssertionResultBehaviour[T]] {

  override def evaluate(context: T): AssertionResultBehaviour[T] =
    Bool(predicate(context)).thenElse(trueExp.evaluate(context), falseExp.evaluate(context))
}
