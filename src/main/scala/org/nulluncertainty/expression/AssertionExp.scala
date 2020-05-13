package org.nulluncertainty.expression

import org.nulluncertainty.assertion.AssertionFailureException

import scala.util.{Either, Failure, Success, Try}

case class AssertionExp[T,E](expression: Expression[T,Bool], otherwise: T => E)
  extends ComposableAssertionExp[T,T,T] {

  override def evaluate(context: T): AssertionResultBehaviour[T] =
    expression.evaluate(context).thenElse(
      AssertionSuccessfulResult(context),
      AssertionFailureResult(List(otherwise(context))))
}

case class SuccessfulAssertionExp[T]()
  extends ComposableAssertionExp[T,T,T] {

  override def evaluate(context: T): AssertionResultBehaviour[T] =
    AssertionSuccessfulResult(context)
}

trait AssertionResultBehaviour[T] extends LogicalOperators[AssertionResultBehaviour[T]] {

  def andSuccessful(result: AssertionSuccessfulResult[T]): AssertionResultBehaviour[T]

  def andFailure[E](result: AssertionFailureResult[T,E]): AssertionResultBehaviour[T]

  def orSuccessful(result: AssertionSuccessfulResult[T]): AssertionResultBehaviour[T]

  def orFailure[E](result: AssertionFailureResult[T,E]): AssertionResultBehaviour[T]

  def toEither[E]: Either[AssertionFailureException[E], T]

  def toTry: Try[T]

  def expectsToBeTrue(): Unit

  def expectsToBeFalseWith(expectedErrorMessages: String*): Unit

  def signalIfFailed(): Unit

  def signalIfFailed[E](throwable: Seq[E] => Throwable): Unit

  def matches[R](partialFunction: PartialFunction[AssertionResultBehaviour[_], R]): R =
    partialFunction.apply(this)

  def flatMap[U](f: T => AssertionResultBehaviour[U]): AssertionResultBehaviour[U]

  def map[U](f: T => U): AssertionResultBehaviour[U]

  def fold[U,E](fa: List[E] => U, fb: T => U): U

  def toOption: Option[T]
}

case class AssertionSuccessfulResult[T](context: T) extends AssertionResultBehaviour[T] {

  override def and(assertionResult: => AssertionResultBehaviour[T]): AssertionResultBehaviour[T] =
    assertionResult.andSuccessful(this)

  override def andSuccessful(result: AssertionSuccessfulResult[T]): AssertionResultBehaviour[T] =
    this

  override def andFailure[E](result: AssertionFailureResult[T,E]): AssertionResultBehaviour[T] =
    result

  override def or(assertionResult: => AssertionResultBehaviour[T]): AssertionResultBehaviour[T] =
    assertionResult.orSuccessful(this)

  override def orSuccessful(result: AssertionSuccessfulResult[T]): AssertionResultBehaviour[T] =
    this

  override def orFailure[E](result: AssertionFailureResult[T,E]): AssertionResultBehaviour[T] =
    this

  override def toEither[E]: Either[AssertionFailureException[E], T] =
    Right(context)

  override def toTry: Try[T] = Success(context)

  override def expectsToBeTrue(): Unit = {}

  override def expectsToBeFalseWith(expectedErrorMessages: String*): Unit =
    throw new RuntimeException("Expected to be false, but is true")

  override def signalIfFailed(): Unit = {}

  override def flatMap[U](f: T => AssertionResultBehaviour[U]): AssertionResultBehaviour[U] =
    f(context)

  override def map[U](f: T => U): AssertionResultBehaviour[U] =
    AssertionSuccessfulResult(f(context))

  override def signalIfFailed[E](throwable: Seq[E] => Throwable): Unit = {}

  override def ifTrue(block: => AssertionResultBehaviour[T]): AssertionResultBehaviour[T] = block

  override def fold[U,E](fa: List[E] => U, fb: T => U): U = fb(context)

  override def toOption: Option[T] = Some(context)

  override def ifFalse(block: => AssertionResultBehaviour[T]): AssertionResultBehaviour[T] = this
}

case class AssertionFailureResult[T,E](errorMessages: List[E]) extends AssertionResultBehaviour[T] {

  override def and(assertionResult: => AssertionResultBehaviour[T]): AssertionResultBehaviour[T] =
    assertionResult.andFailure(this)

  override def andSuccessful(result: AssertionSuccessfulResult[T]): AssertionResultBehaviour[T] =
    this

  override def andFailure[E](result: AssertionFailureResult[T,E]): AssertionResultBehaviour[T] =
    AssertionFailureResult(result.errorMessages ++ errorMessages)

  override def or(assertionResult: => AssertionResultBehaviour[T]): AssertionResultBehaviour[T] =
    assertionResult.orFailure(this)

  override def orSuccessful(result: AssertionSuccessfulResult[T]): AssertionResultBehaviour[T] =
    result

  override def orFailure[E](result: AssertionFailureResult[T,E]): AssertionResultBehaviour[T] =
    AssertionFailureResult(result.errorMessages ++ errorMessages)

  override def toEither[E]: Either[AssertionFailureException[E], T] =
    Left(AssertionFailureException(errorMessages.map(_.asInstanceOf[E])))

  override def toTry: Try[T] = Failure(AssertionFailureException(errorMessages))

  override def expectsToBeTrue(): Unit =
    throw new RuntimeException(s"Expected to be true, but got false with ${errorMessages.mkString(", ")}")

  override def expectsToBeFalseWith(expectedErrorMessages: String*): Unit =
    if (expectedErrorMessages != errorMessages)
      throw new RuntimeException(s"Expected $expectedErrorMessages but got $errorMessages")

  override def signalIfFailed(): Unit =
    throw AssertionFailureException(errorMessages)

  override def flatMap[U](f: T => AssertionResultBehaviour[U]): AssertionResultBehaviour[U] =
    this.asInstanceOf[AssertionResultBehaviour[U]]

  override def map[U](f: T => U): AssertionResultBehaviour[U] =
    this.asInstanceOf[AssertionResultBehaviour[U]]

  override def signalIfFailed[E](throwable: Seq[E] => Throwable): Unit =
    throw throwable(errorMessages.map(_.asInstanceOf[E]))

  override def ifTrue(block: => AssertionResultBehaviour[T]): AssertionResultBehaviour[T] = this

  override def fold[U,E](fa: List[E] => U, fb: T => U): U = fa(errorMessages.map(_.asInstanceOf[E]))

  override def toOption: Option[T] = None

  override def ifFalse(block: => AssertionResultBehaviour[T]): AssertionResultBehaviour[T] = block
}
