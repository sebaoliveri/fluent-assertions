package org.validify.expression

case class IfFalseExp[T,R <: LogicalOperators[R]](left: LogicalOperatorsExp[T,R],
                                                  right: LogicalOperatorsExp[T,R])
                                                  extends LogicalOperatorsExp[T,R] {

  override def evaluate(context: T): R =
    left.evaluate(context).ifFalse(right.evaluate(context))
}
