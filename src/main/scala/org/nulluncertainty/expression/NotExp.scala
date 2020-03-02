package org.nulluncertainty.expression

case class NotExp[T](expression: Expression[T,Bool]) extends ComposableBooleanExp[T] {

  override def evaluate(context: T): Bool =
    expression.evaluate(context).not()
}
