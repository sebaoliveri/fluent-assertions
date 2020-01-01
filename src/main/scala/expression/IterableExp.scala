package expression

import expression.QuantifiableOrderedExp.intVariable
import IterableExp._

object IterableExp {

  def iterableConstant[T,R](iterable: Iterable[R]): IterableExp[T,R] =
    iterableVariable(_ => iterable)

  def iterableVariable[T,R](f: T => Iterable[R]): IterableExp[T,R] =
    IterableExp(f)
}

case class IterableExp[T,R](func: T => Iterable[R]) extends TypeExp[T,Iterable[R]](func) {

  import ObjectExp._

  private val respectOrder: Int => R => Int => R => LogicalOperatorsExp[Unit,Bool] = i1 => r1 => i2 => r2 =>
    objectConstant(r1).isEqualTo(objectConstant(r2)).and(objectConstant(i1).isEqualTo(objectConstant(i2)))
  private val ignoreOrder: Int => R => Int => R => LogicalOperatorsExp[Unit,Bool] = _ => r1 => _ => r2 =>
    objectConstant(r1).isEqualTo(objectConstant(r2))

  def forAll(predicate: Int => R => LogicalOperatorsExp[Unit,Bool]): LogicalOperatorsExp[T,Bool] =
    FoldIterableExp(this, predicate, TrueExp, _ and _)

  def existAny(predicate: Int => R => LogicalOperatorsExp[Unit,Bool]): LogicalOperatorsExp[T,Bool] =
    FoldIterableExp(this, predicate, FalseExp, _ or _)

  def containsAllInSameOrder(theseObjects: IterableExp[T,R]): LogicalOperatorsExp[T,Bool] =
    ContainsAllIterableExp(this, theseObjects, _ and _, respectOrder)

  def containsSomeInSameOrder(theseObjects: IterableExp[T,R]): LogicalOperatorsExp[T,Bool] =
    ContainsSomeIterableExp(this, theseObjects, _ and _, respectOrder)

  def containsAll(theseObjects: IterableExp[T,R]): LogicalOperatorsExp[T,Bool] =
    ContainsAllIterableExp(this, theseObjects, _ and _, ignoreOrder)

  def doesNotContainAnyOf(theseObjects: IterableExp[T,R]): LogicalOperatorsExp[T,Bool] =
    NotExp(containsAnyOf(theseObjects))

  def containsAnyOf(theseObjects: IterableExp[T,R]): LogicalOperatorsExp[T,Bool] =
    ContainsAllIterableExp(this, theseObjects, _ or _, ignoreOrder)

  def contains(anObject: ObjectExp[T,R]): LogicalOperatorsExp[T,Bool] =
    containsAll(IterableExp(anObject.func.andThen(Seq(_))))

  def doesNotContain(anObject: ObjectExp[T,R]): LogicalOperatorsExp[T,Bool] =
    NotExp(contains(anObject))

  def containsNoDuplicates: LogicalOperatorsExp[T,Bool] =
    intVariable(func.andThen(_.size)).isEqualTo(QuantifiableExp(func.andThen(_.toSet).andThen(_.size)))

  def containsNoDuplicatesMatching[R1](criteria: R => R1): LogicalOperatorsExp[T,Bool] = // TODO matching?
    intVariable(func.andThen(_.size)).isEqualTo(QuantifiableExp(func.andThen(_.map(criteria)).andThen(_.toSet).andThen(_.size)))

  def isEmpty: LogicalOperatorsExp[T,Bool] =
    IsQuantifiableEqualToExp(intVariable(func.andThen(_.size)), QuantifiableExp(_ => 0))

  def isNotEmpty: LogicalOperatorsExp[T,Bool] = NotExp(isEmpty)

  override def evaluate(context: T): Iterable[R] = func(context)
}

case class FoldIterableExp[T,R](iterableExp: IterableExp[T,R],
                                predicate: Int => R => LogicalOperatorsExp[Unit,Bool],
                                seed: Bool,
                                operator: (Bool,Bool) => Bool) extends LogicalOperatorsExp[T,Bool] {

  override def evaluate(context: T): Bool = {
    iterableExp.evaluate(context).zipWithIndex.foldLeft[Bool](seed)
      { case (result,(iterated,index)) => operator(result, predicate(index)(iterated).evaluate()) }
  }
}

case class ContainsAllIterableExp[T,R](iterableExp: IterableExp[T,R],
                                       subIterableExp: IterableExp[T,R],
                                       operator: (LogicalOperatorsExp[T, Bool],LogicalOperatorsExp[T, Bool]) => LogicalOperatorsExp[T, Bool],
                                       predicate: Int => R => Int => R => LogicalOperatorsExp[Unit,Bool]) extends LogicalOperatorsExp[T,Bool] {

  override def evaluate(context: T): Bool = {
    subIterableExp.evaluate(context).zipWithIndex.foldLeft[LogicalOperatorsExp[T,Bool]](NullExp[T,Bool]())
      { case (result,(iterated,index)) => operator(result, iterableExp.existAny(predicate(index)(iterated))) }
    .evaluate(context)
  }
}

case class ContainsSomeIterableExp[T,R](iterableExp: IterableExp[T,R],
                                        subIterableExp: IterableExp[T,R],
                                        operator: (LogicalOperatorsExp[T, Bool],LogicalOperatorsExp[T, Bool]) => LogicalOperatorsExp[T, Bool],
                                        predicate: Int => R => Int => R => LogicalOperatorsExp[Unit,Bool]) extends LogicalOperatorsExp[T,Bool] {

  override def evaluate(context: T): Bool = {
    @scala.annotation.tailrec
    def contains(iterable: Iterable[R], bool: Bool): Bool =
      iterable.toList match {
        case Nil => bool
        case _ :: tail => contains(tail, bool.or(isIn(subIterableExp.evaluate(context), iterableConstant(tail)).evaluate(context)))
      }
    contains(iterableExp.evaluate(context), isIn(subIterableExp.evaluate(context), iterableExp).evaluate(context))
  }

  private def isIn(aSubIterable: Iterable[R], anIterableExp: IterableExp[T,R]): LogicalOperatorsExp[T,Bool] =
    ContainsAllIterableExp(anIterableExp, IterableExp(_ => aSubIterable), operator, predicate)
}
