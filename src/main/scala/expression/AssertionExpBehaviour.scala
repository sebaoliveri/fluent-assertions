package expression

import assertion.AssertionFailureException

import scala.util.{Either, Try}

trait AssertionExpBehaviour[P1,P2,P3] extends LogicalOperatorsExp[P1,AssertionResultBehaviour[P3]] {

  def and(expression: LogicalOperatorsExp[P1,AssertionResultBehaviour[P3]]) =
    new AndExp(left = this, right = expression) with AssertionExpBehaviour[P1,P2,P3]

  def or(expression: LogicalOperatorsExp[P1,AssertionResultBehaviour[P3]]) =
    new OrExp(left = this, right = expression) with AssertionExpBehaviour[P1,P2,P3]

  def ifTrue(expression: LogicalOperatorsExp[P1,AssertionResultBehaviour[P3]]) =
    new IfTrueExp(left = this, right = expression) with AssertionExpBehaviour[P1,P2,P3]

  def ifFalse(expression: LogicalOperatorsExp[P1,AssertionResultBehaviour[P3]]) =
    new IfFalseExp(left = this, right = expression) with AssertionExpBehaviour[P1,P2,P3]

  def map[P4](f: P3 => P4): MapAssertionExp[P1, P3, P4] =
    MapAssertionExp(this, f)

  def flatMap[P4](f: P3 => LogicalOperatorsExp[P3,AssertionResultBehaviour[P4]]): FlatMapAssertionExp[P1,P3,P4] =
    FlatMapAssertionExp(this, f)

  def evaluate: AssertionResultBehaviour[P3] =
    evaluate(new Object().asInstanceOf[P1])

  def toEither: Either[AssertionFailureException,P3] =
    evaluate.toEither

  def toTry: Try[P3] =
    evaluate.toTry

  def signalIfFailed(): Unit =
    evaluate.signalIfFailed()

  def signalIfFailed(throwable: Seq[String] => Throwable): Unit =
    evaluate.signalIfFailed(throwable)

  def matches[R](partialFunction: PartialFunction[AssertionResultBehaviour[_], R]): R =
    evaluate.matches(partialFunction)

  def expectsToBeTrue(): Unit =
    evaluate.expectsToBeTrue()

  def expectsToBeFalseWith(errorMessages: String*): Unit =
    evaluate.expectsToBeFalseWith(errorMessages:_*)
}
