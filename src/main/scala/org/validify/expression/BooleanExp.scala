package org.validify.expression

object BooleanExp {

  def boolConstant[T](bool: => Boolean): BooleanExp[T] =
    boolVariable(_ => bool)

  def boolVariable[T](f: T => Boolean): BooleanExp[T] =
    BooleanExp(f)
}

case class BooleanExp[T](func: T => Boolean) extends AnyExp[T,Boolean](func) {

  def isTrue: IsEqualToExp[T,Boolean] = IsEqualToExp(ObjectExp(func), ObjectExp((_:T) => true))

  def isFalse: NotExp[T] = NotExp(isTrue)
}

case class TrueBooleanExp[T]() extends BoolExpBehaviour[T] {

  override def evaluate(context: T): Bool = TrueExp
}

case class FalseBooleanExp[T]() extends BoolExpBehaviour[T] {

  override def evaluate(context: T): Bool = FalseExp
}
