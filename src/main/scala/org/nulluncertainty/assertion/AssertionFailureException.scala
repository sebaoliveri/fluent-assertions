package org.nulluncertainty.assertion

case class AssertionFailureException[E](errors: List[E]) extends RuntimeException
