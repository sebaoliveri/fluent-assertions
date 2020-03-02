package org.nulluncertainty.expression

case class ConditionalAssertionExp[T](predicate: ComposableBooleanExp[T],
                                      trueExp: Expression[T,AssertionResultBehaviour[T]],
                                      falseExp: Expression[T,AssertionResultBehaviour[T]])
                                      extends ComposableAssertionExp[T,T,T] {

  override def evaluate(context: T): AssertionResultBehaviour[T] =
    predicate.evaluate(context)
      .thenElse(
        trueExp.evaluate(context),
        falseExp.evaluate(context))
}
