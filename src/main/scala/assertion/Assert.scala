package assertion

import expression.{AssertionResultBehaviour, Expression}

import scala.util.{Either, Try}

object Assert {
  def assert[T](expression: Expression[T,AssertionResultBehaviour[T]]): Assert[T] = Assert(expression)
}

case class Assert[T](expression: Expression[T,AssertionResultBehaviour[T]]) {

  private val NoContext = new Object().asInstanceOf[T]

  def toEither: Either[AssertionFailureException, T] = inNoContext.toEither

  def toTry: Try[T] = inNoContext.toTry

  def signalIfFailed(): Unit = inNoContext.signalIfFailed()

  def matches[R](partialFunction: PartialFunction[AssertionResultBehaviour[_], R]): R =
    inNoContext.matches(partialFunction)

  def in(context: T): AssertionResultBehaviour[T] = expression.evaluate(context)

  def expectsToBeTrue(): Unit = inNoContext.expectsToBeTrue()

  def expectsToBeFalseWith(errorMessages: String*): Unit =
    inNoContext.expectsToBeFalseWith(errorMessages:_*)

  def inNoContext: AssertionResultBehaviour[T] = in(NoContext)
}
