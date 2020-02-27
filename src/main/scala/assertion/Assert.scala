package assertion

import expression.{AssertionResultBehaviour, LogicalOperatorsExp, MapAssertionExp, SuccessfulAssertionExp}

import scala.util.{Either, Try}

object Assert {

  def assert[T](expression: LogicalOperatorsExp[T,AssertionResultBehaviour[T]]): Assert[T] = Assert(expression)

  def truth[T]: Assert[T] = Assert(SuccessfulAssertionExp())
}

case class Assert[T](expression: LogicalOperatorsExp[T,AssertionResultBehaviour[T]]) {

  private val NoContext = new Object().asInstanceOf[T]

  def toEither: Either[AssertionFailureException,T] = inNoContext.toEither

  def toTry: Try[T] = inNoContext.toTry

  def signalIfFailed(): Unit = inNoContext.signalIfFailed()

  def signalIfFailed(throwable: Seq[String] => Throwable): Unit = inNoContext.signalIfFailed(throwable)

  def matches[R](partialFunction: PartialFunction[AssertionResultBehaviour[_], R]): R =
    inNoContext.matches(partialFunction)

  def in(context: T): AssertionResultBehaviour[T] = expression.evaluate(context)

  def expectsToBeTrue(): Unit = inNoContext.expectsToBeTrue()

  def expectsToBeFalseWith(errorMessages: String*): Unit =
    inNoContext.expectsToBeFalseWith(errorMessages:_*)

  def inNoContext: AssertionResultBehaviour[T] = in(NoContext)

  def ifTrue(anotherAssert: Assert[T]): Assert[T] =
    copy(expression = expression.ifTrue(anotherAssert.expression))

  def ifFalse(anotherAssert: Assert[T]): Assert[T] =
    copy(expression = expression.ifFalse(anotherAssert.expression))

  def and(anotherAssert: Assert[T]): Assert[T] =
    copy(expression = expression.and(anotherAssert.expression))

  def or(anotherAssert: Assert[T]): Assert[T] =
    copy(expression = expression.or(anotherAssert.expression))

  def ifTrueAssert(anExpression: LogicalOperatorsExp[T,AssertionResultBehaviour[T]]): Assert[T] =
    copy(expression = expression.ifTrue(anExpression))

  def ifFalseAssert(anExpression: LogicalOperatorsExp[T,AssertionResultBehaviour[T]]): Assert[T] =
    copy(expression = expression.ifFalse(anExpression))

  def andAssert(anExpression: LogicalOperatorsExp[T,AssertionResultBehaviour[T]]): Assert[T] =
    copy(expression = expression.and(anExpression))

  def orAssert(anExpression: LogicalOperatorsExp[T,AssertionResultBehaviour[T]]): Assert[T] =
    copy(expression = expression.or(anExpression))

  def map[U](f: T => U): MapAssertionExp[T, T, U] =
    MapAssertionExp(expression, f)
}
