package org.validify.expression

abstract class AnyExp[T,R](func: T => R) extends Expression[T,R] {

  def isEqualTo(another: AnyExp[T,R]): IsEqualToExp[T,R] =
    IsEqualToExp(this, another)

  override def evaluate(context: T): R =
    func(context)
}

case class IsEqualToExp[T,R](left: AnyExp[T,R], right: AnyExp[T,R]) extends ComposableBooleanExp[T] {

  override def evaluate(context: T): Bool =
    Bool(left.evaluate(context) == right.evaluate(context))
}
