package expression

import expression.QuantifiableOrderedExp.intVariable

object IterableExp {

  def iterableConstant[T,R](iterable: => Iterable[R]): IterableExp[T,R] =
    iterableVariable(_ => iterable)

  def iterableVariable[T,R](f: T => Iterable[R]): IterableExp[T,R] =
    IterableExp(f)
}

case class IterableExp[T,R](func: T => Iterable[R]) extends TypeExp[T,Iterable[R]](func) {

  private val and: (Bool,Bool) => Bool = _ and _
  private val or: (Bool,Bool) => Bool = _ or _

  def forAll(predicate: R => Boolean): BooleanExp[T,Bool] = FoldBoolExp(this, predicate, and)

  def existAny(predicate: R => Boolean): BooleanExp[T,Bool] = FoldBoolExp(this, predicate, or)

  def isNotEmpty: BooleanExp[T,Bool] =
    IsGreaterThanExp(intVariable(func.andThen(_.size)), QuantifiableExp(_ => 0))

  def isEmpty: BooleanExp[T,Bool] =
    NotExp(isNotEmpty)

  override def evaluate(context: T): Iterable[R] = func(context)
}

case class FoldBoolExp[T,R](iterableExp: IterableExp[T,R], predicate: R => Boolean, operator: (Bool,Bool) => Bool)
  extends BooleanExp[T,Bool] {

  override def evaluate(context: T): Bool =
    iterableExp.evaluate(context).foldLeft[Bool](TrueExp) { case (result, iterated) =>
      operator(result, Bool(predicate(iterated)))
    }
}
