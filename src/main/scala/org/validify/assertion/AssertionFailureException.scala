package org.validify.assertion

case class AssertionFailureException(errorMessages: List[String]) extends RuntimeException {
  override def getMessage: String = errorMessages.mkString(", ")
}
