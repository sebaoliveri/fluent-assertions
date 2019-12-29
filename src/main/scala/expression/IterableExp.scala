package expression

import expression.QuantifiableOrderedExp.intVariable
import IterableExp._

object IterableExp {

  def iterableConstant[T,R](iterable: collection.immutable.Iterable[R]): IterableExp[T,R] =
    iterableVariable(_ => iterable)

  def iterableVariable[T,R](f: T => collection.immutable.Iterable[R]): IterableExp[T,R] =
    IterableExp(f)
}

case class IterableExp[T,R](func: T => collection.immutable.Iterable[R]) extends TypeExp[T,collection.immutable.Iterable[R]](func) {

  private val respectOrder: (R,Int) => (R,Int) => Boolean = (r,i) => _ == r && _ == i
  private val ignoreOrder: (R,Int) => (R,Int) => Boolean = (r:R,_) => (r1:R,_) => r1 == r

  def forAll(predicate: (R,Int) => Boolean): BooleanExp[T,Bool] =
    FoldIterableExp(this, predicate, TrueExp, _ and _)

  def existAny(predicate: (R,Int) => Boolean): BooleanExp[T,Bool] =
    FoldIterableExp(this, predicate, FalseExp, _ or _)

  def containsAllInSameOrder(theseObjects: IterableExp[T,R]): BooleanExp[T,Bool] =
    ContainsAllIterableExp(this, theseObjects, _ and _, respectOrder)

  def containsSomeInSameOrder(theseObjects: IterableExp[T,R]): BooleanExp[T,Bool] =
    ContainsSomeIterableExp(this, theseObjects, _ and _, respectOrder)

  def containsAll(theseObjects: IterableExp[T,R]): BooleanExp[T,Bool] =
    ContainsAllIterableExp(this, theseObjects, _ and _, ignoreOrder)

  def doesNotContainAnyOf(theseObjects: IterableExp[T,R]): BooleanExp[T,Bool] =
    NotExp(containsAnyOf(theseObjects))

  def containsAnyOf(theseObjects: IterableExp[T,R]): BooleanExp[T,Bool] =
    ContainsAllIterableExp(this, theseObjects, _ or _, ignoreOrder)

  def contains(anObject: ObjectExp[T,R]): BooleanExp[T,Bool] =
    containsAll(IterableExp(anObject.func.andThen(Seq(_))))

  def doesNotContain(anObject: ObjectExp[T,R]): BooleanExp[T,Bool] =
    NotExp(contains(anObject))

  def containsNoDuplicates: BooleanExp[T,Bool] =
    intVariable(func.andThen(_.size)).isEqualTo(QuantifiableExp(func.andThen(_.toSet).andThen(_.size)))

  def containsNoDuplicatesMatching[R1](criteria: R => R1): BooleanExp[T,Bool] = // TODO matching?
    intVariable(func.andThen(_.size)).isEqualTo(QuantifiableExp(func.andThen(_.map(criteria)).andThen(_.toSet).andThen(_.size)))

  def isEmpty: BooleanExp[T,Bool] =
    IsQuantifiableEqualToExp(intVariable(func.andThen(_.size)), QuantifiableExp(_ => 0))

  def isNotEmpty: BooleanExp[T,Bool] = NotExp(isEmpty)

  override def evaluate(context: T): collection.immutable.Iterable[R] = func(context)
}

case class FoldIterableExp[T,R](iterableExp: IterableExp[T,R],
                                predicate: (R,Int) => Boolean,
                                seed: Bool,
                                operator: (Bool,Bool) => Bool) extends BooleanExp[T,Bool] {

  override def evaluate(context: T): Bool = {
    iterableExp.evaluate(context).zipWithIndex.foldLeft[Bool](seed)
      { case (result,(iterated,index)) => operator(result, Bool(predicate(iterated, index))) }
  }
}

case class ContainsAllIterableExp[T,R](iterableExp: IterableExp[T,R],
                                        subIterableExp: IterableExp[T,R],
                                        operator: (BooleanExp[T, Bool],BooleanExp[T, Bool]) => BooleanExp[T, Bool],
                                        predicate: (R,Int) => (R,Int) => Boolean) extends BooleanExp[T,Bool] {

  override def evaluate(context: T): Bool = {
    subIterableExp.evaluate(context).zipWithIndex.foldLeft[BooleanExp[T,Bool]](NullBooleanExp[T,Bool]())
      { case (result,(iterated,index)) => operator(result, iterableExp.existAny(predicate(iterated, index))) }
    .evaluate(context)
  }
}

case class ContainsSomeIterableExp[T,R](iterableExp: IterableExp[T,R],
                                    subIterableExp: IterableExp[T,R],
                                    operator: (BooleanExp[T, Bool],BooleanExp[T, Bool]) => BooleanExp[T, Bool],
                                    predicate: (R,Int) => (R,Int) => Boolean) extends BooleanExp[T,Bool] {

  override def evaluate(context: T): Bool = {
    @scala.annotation.tailrec
    def contains(iterable: collection.immutable.Iterable[R], bool: Bool): Bool =
      iterable.toList match {
        case Nil => bool
        case _ :: tail => contains(tail, bool.or(isIn(subIterableExp.evaluate(context), iterableConstant(tail)).evaluate(context)))
      }
    contains(iterableExp.evaluate(context), isIn(subIterableExp.evaluate(context), iterableExp).evaluate(context))
  }

  private def isIn(aSubIterable: collection.immutable.Iterable[R], anIterableExp: IterableExp[T,R]): BooleanExp[T,Bool] =
    ContainsAllIterableExp(anIterableExp, IterableExp(_ => aSubIterable), operator, predicate)
}
