package org.nulluncertainty.expression

case class AndExp[T,R <: LogicalOperators[R]](left: Expression[T,R],
                                              right: Expression[T,R])
                                              extends Expression[T,R] {

  override def evaluate(context: T): R =
    left.evaluate(context).and(right.evaluate(context))
}
