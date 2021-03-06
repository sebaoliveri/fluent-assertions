package org.nulluncertainty.expression

case class IfFalseExp[T,R <: LogicalOperators[R]](left: Expression[T,R],
                                                  right: Expression[T,R])
                                                  extends Expression[T,R] {

  override def evaluate(context: T): R =
    left.evaluate(context).ifFalse(right.evaluate(context))
}
