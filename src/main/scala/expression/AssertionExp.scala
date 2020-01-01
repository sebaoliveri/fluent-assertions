package expression

import assertion.AssertionFailureException

import scala.util.{Either, Failure, Success, Try}

case class AssertionExp[T](expression: Expression[T,Bool], otherwise: T => String) extends LogicalOperatorsExp[T,AssertionResultBehaviour[T]] {

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

  def toEither: Either[AssertionFailureException, T]

  def toTry: Try[T]

  def expectsToBeTrue(): Unit

  def expectsToBeFalseWith(expectedErrorMessages: String*): Unit

  def signalIfFailed(): Unit

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

  override def toEither: Either[AssertionFailureException, T] =
    Right(context)

  override def toTry: Try[T] = Success(context)

  override def expectsToBeTrue(): Unit = {}

  override def expectsToBeFalseWith(expectedErrorMessages: String*): Unit =
    throw new RuntimeException("Expected to be false, but is true")

  override def signalIfFailed(): Unit = {}
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

  override def toEither: Either[AssertionFailureException, T] =
    Left(AssertionFailureException(errorMessages))

  override def toTry: Try[T] = Failure(AssertionFailureException(errorMessages))

  override def expectsToBeTrue(): Unit =
    throw new RuntimeException(s"Expected to be true, but got false with ${errorMessages.mkString(", ")}")

  override def expectsToBeFalseWith(expectedErrorMessages: String*): Unit =
    if (expectedErrorMessages != errorMessages)
      throw new RuntimeException(s"Expected $expectedErrorMessages but got $errorMessages")

  override def signalIfFailed(): Unit =
    throw AssertionFailureException(errorMessages)
}
