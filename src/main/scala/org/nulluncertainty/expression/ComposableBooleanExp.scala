package org.nulluncertainty.expression

trait ComposableBooleanExp[T] extends Expression[T,Bool] {

  def and(expression: ComposableBooleanExp[T]): ComposableBooleanExp[T] =
    new AndExp(left = this, right = expression) with ComposableBooleanExp[T]

  def or(expression: ComposableBooleanExp[T]): ComposableBooleanExp[T] =
    new OrExp(left = this, right = expression) with ComposableBooleanExp[T]

  def ifTrue(expression: ComposableBooleanExp[T]): ComposableBooleanExp[T] =
    new IfTrueExp(left = this, right = expression) with ComposableBooleanExp[T]

  def ifFalse(expression: ComposableBooleanExp[T]): ComposableBooleanExp[T] =
    new IfFalseExp(left = this, right = expression) with ComposableBooleanExp[T]

  def thenElse(trueExp: Expression[T,AssertionResultBehaviour[T]],
               falseExp: Expression[T,AssertionResultBehaviour[T]]): ConditionalAssertionExp[T] =
    ConditionalAssertionExp(this, trueExp, falseExp)

  def evaluate: Bool =
    evaluate(new Object().asInstanceOf[T])
}
