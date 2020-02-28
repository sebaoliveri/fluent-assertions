package expression

import expression.QuantifiableOrderedExp.intVariable
import IterableExp._

object IterableExp {

  def iterableConstant[T,R](iterable: Iterable[R]): IterableExp[T,R] =
    iterableVariable(_ => iterable)

  def iterableVariable[T,R](f: T => Iterable[R]): IterableExp[T,R] =
    IterableExp(f)
}

case class IterableExp[T,R](func: T => Iterable[R]) extends AnyExp[T,Iterable[R]](func) {

  import ObjectExp._

  private val respectOrder: Int => R => Int => R => BoolExpBehaviour[Unit] = i1 => r1 => i2 => r2 =>
    objectConstant(r1).isEqualTo(objectConstant(r2)).and(objectConstant(i1).isEqualTo(objectConstant(i2)))
  private val ignoreOrder: Int => R => Int => R => BoolExpBehaviour[Unit] = _ => r1 => _ => r2 =>
    objectConstant(r1).isEqualTo(objectConstant(r2))

  def forAll(predicate: Int => R => BoolExpBehaviour[Unit]): BoolExpBehaviour[T] =
    FoldIterableExp(this, predicate, TrueExp, _ and _)

  def existAny(predicate: Int => R => BoolExpBehaviour[Unit]): BoolExpBehaviour[T] =
    FoldIterableExp(this, predicate, FalseExp, _ or _)

  def containsAllInSameOrder(theseObjects: IterableExp[T,R]): BoolExpBehaviour[T] =
    ContainsAllIterableExp(this, theseObjects, _ and _, respectOrder)

  def containsSomeInSameOrder(theseObjects: IterableExp[T,R]): BoolExpBehaviour[T] =
    ContainsSomeIterableExp(this, theseObjects, _ and _, respectOrder)

  def containsAll(theseObjects: IterableExp[T,R]): BoolExpBehaviour[T] =
    ContainsAllIterableExp(this, theseObjects, _ and _, ignoreOrder)

  def doesNotContainAnyOf(theseObjects: IterableExp[T,R]): BoolExpBehaviour[T] =
    NotExp(containsAnyOf(theseObjects))

  def containsAnyOf(theseObjects: IterableExp[T,R]): BoolExpBehaviour[T] =
    ContainsAllIterableExp(this, theseObjects, _ or _, ignoreOrder)

  def contains(anObject: ObjectExp[T,R]): BoolExpBehaviour[T] =
    containsAll(IterableExp(anObject.func.andThen(Seq(_))))

  def doesNotContain(anObject: ObjectExp[T,R]): BoolExpBehaviour[T] =
    NotExp(contains(anObject))

  def containsNoDuplicates: BoolExpBehaviour[T] =
    intVariable(func.andThen(_.size)).isEqualTo(QuantifiableExp(func.andThen(_.toSet).andThen(_.size)))

  def isEmpty: BoolExpBehaviour[T] =
    IsQuantifiableEqualToExp(intVariable(func.andThen(_.size)), QuantifiableExp(_ => 0))

  def isNotEmpty: BoolExpBehaviour[T] = NotExp(isEmpty)
}

case class FoldIterableExp[T,R](iterableExp: IterableExp[T,R],
                                predicate: Int => R => BoolExpBehaviour[Unit],
                                seed: Bool,
                                operator: (Bool,Bool) => Bool) extends BoolExpBehaviour[T] {

  override def evaluate(context: T): Bool = {
    iterableExp.evaluate(context).zipWithIndex.foldLeft[Bool](seed)
      { case (result,(iterated,index)) => operator(result, predicate(index)(iterated).evaluate()) }
  }
}

case class ContainsAllIterableExp[T,R](iterableExp: IterableExp[T,R],
                                       subIterableExp: IterableExp[T,R],
                                       operator: (BoolExpBehaviour[T],BoolExpBehaviour[T]) => BoolExpBehaviour[T],
                                       predicate: Int => R => Int => R => BoolExpBehaviour[Unit]) extends BoolExpBehaviour[T] {

  override def evaluate(context: T): Bool = {
    subIterableExp.evaluate(context).zipWithIndex.foldLeft[BoolExpBehaviour[T]](NullExp[T,Bool]())
      { case (result,(iterated,index)) => operator(result, iterableExp.existAny(predicate(index)(iterated))) }
    .evaluate(context)
  }
}

case class ContainsSomeIterableExp[T,R](iterableExp: IterableExp[T,R],
                                        subIterableExp: IterableExp[T,R],
                                        operator: (BoolExpBehaviour[T],BoolExpBehaviour[T]) => BoolExpBehaviour[T],
                                        predicate: Int => R => Int => R => BoolExpBehaviour[Unit]) extends BoolExpBehaviour[T] {

  override def evaluate(context: T): Bool = {
    @scala.annotation.tailrec
    def contains(iterable: Iterable[R], bool: Bool): Bool =
      iterable.toList match {
        case Nil => bool
        case _ :: tail => contains(tail, bool.or(isIn(subIterableExp.evaluate(context), iterableConstant(tail)).evaluate(context)))
      }
    contains(iterableExp.evaluate(context), isIn(subIterableExp.evaluate(context), iterableExp).evaluate(context))
  }

  private def isIn(aSubIterable: Iterable[R], anIterableExp: IterableExp[T,R]): BoolExpBehaviour[T] =
    ContainsAllIterableExp(anIterableExp, IterableExp(_ => aSubIterable), operator, predicate)
}
