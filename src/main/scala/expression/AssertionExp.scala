package expression

import scala.util.Either

case class AssertionExp[T](expression: Expression[T,Bool], otherwise: T => String) extends BooleanExp[T,AssertionResultBehaviour[T]] {

  override def evaluate(context: T): AssertionResultBehaviour[T] =
    expression.evaluate(context).thenElse(
      AssertionSuccessfulResult(context),
      AssertionFailureResult(List(otherwise(context))))
}

trait AssertionResultBehaviour[T] extends LogicalOperators[AssertionResultBehaviour[T]] {

  def andSuccessful(result: AssertionSuccessfulResult[T]): AssertionResultBehaviour[T]

  def andFailure(result: AssertionFailureResult[T]): AssertionResultBehaviour[T]

  def orSuccessful(result: AssertionSuccessfulResult[T]): AssertionResultBehaviour[T]

  def orFailure(result: AssertionFailureResult[T]): AssertionResultBehaviour[T]

  def toEither: Either[List[String], T]

  def map[T1](f: T => T1): AssertionResultBehaviour[T1]

  def expectsToBeTrue(): Unit

  def expectsToBeFalseWith(expectedErrorMessages: String*): Unit

  def signalIfFailed(exception: List[String] => Throwable): Unit

  def matches[R](partialFunction: PartialFunction[AssertionResultBehaviour[_], R]): R =
    partialFunction.apply(this)
}

case class AssertionSuccessfulResult[T](context: T) extends AssertionResultBehaviour[T] {

  override def and(assertionResult: => AssertionResultBehaviour[T]): AssertionResultBehaviour[T] =
    assertionResult.andSuccessful(this)

  override def andSuccessful(result: AssertionSuccessfulResult[T]): AssertionResultBehaviour[T] =
    this

  override def andFailure(result: AssertionFailureResult[T]): AssertionResultBehaviour[T] =
    result

  override def or(assertionResult: => AssertionResultBehaviour[T]): AssertionResultBehaviour[T] =
    assertionResult.orSuccessful(this)

  override def orSuccessful(result: AssertionSuccessfulResult[T]): AssertionResultBehaviour[T] =
    this

  override def orFailure(result: AssertionFailureResult[T]): AssertionResultBehaviour[T] =
    this

  override def toEither: Either[List[String], T] =
    Right(context)

  override def map[T1](f: T => T1): AssertionResultBehaviour[T1] =
    AssertionSuccessfulResult(f(context))

  override def expectsToBeTrue(): Unit = {}

  override def expectsToBeFalseWith(expectedErrorMessages: String*): Unit =
    throw new RuntimeException("Expected to be false, but is true")

  override def signalIfFailed(exception: List[String] => Throwable): Unit = {}
}

case class AssertionFailureResult[T](errorMessages: List[String]) extends AssertionResultBehaviour[T] {

  override def and(assertionResult: => AssertionResultBehaviour[T]): AssertionResultBehaviour[T] =
    assertionResult.andFailure(this)

  override def andSuccessful(result: AssertionSuccessfulResult[T]): AssertionResultBehaviour[T] =
    this

  override def andFailure(result: AssertionFailureResult[T]): AssertionResultBehaviour[T] =
    AssertionFailureResult(result.errorMessages ++ errorMessages)

  override def or(assertionResult: => AssertionResultBehaviour[T]): AssertionResultBehaviour[T] =
    assertionResult.orFailure(this)

  override def orSuccessful(result: AssertionSuccessfulResult[T]): AssertionResultBehaviour[T] =
    result

  override def orFailure(result: AssertionFailureResult[T]): AssertionResultBehaviour[T] =
    AssertionFailureResult(result.errorMessages ++ errorMessages)

  override def toEither: Either[List[String], T] =
    Left(errorMessages)

  override def map[T1](f: T => T1): AssertionResultBehaviour[T1] =
    AssertionFailureResult(errorMessages)

  override def expectsToBeTrue(): Unit =
    throw new RuntimeException(s"Expected to be true, but got false with ${errorMessages.mkString(", ")}")

  override def expectsToBeFalseWith(expectedErrorMessages: String*): Unit =
    if (expectedErrorMessages != errorMessages)
      throw new RuntimeException(s"Expected $expectedErrorMessages but got $errorMessages")

  override def signalIfFailed(exception: List[String] => Throwable): Unit =
    throw exception(errorMessages)
}
