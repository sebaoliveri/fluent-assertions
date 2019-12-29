package assertion

case class AssertionFailureException(errorMessages: List[String]) extends RuntimeException
