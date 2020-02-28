package org.validify.expression

case class NotExp[T](expression: Expression[T,Bool]) extends BoolExpBehaviour[T] {

  override def evaluate(context: T): Bool =
    expression.evaluate(context).not()
}
