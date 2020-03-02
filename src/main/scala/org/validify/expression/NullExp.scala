package org.validify.expression

case class NullExp[T,R <: LogicalOperators[R]]() extends ComposableBooleanExp[T] {

  override def and(expression: ComposableBooleanExp[T]): ComposableBooleanExp[T] =
    expression

  override def or(expression: ComposableBooleanExp[T]): ComposableBooleanExp[T] =
    expression

  override def evaluate(context: T): Bool = ??? // do nothing
}
