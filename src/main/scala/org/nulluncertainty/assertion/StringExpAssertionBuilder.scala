package org.nulluncertainty.assertion

import org.nulluncertainty.expression._

object StringExpAssertionBuilder {
  import StringExp._

  def fromStringConstant[T](string: String): StringExpAssertionBuilder[T]=
    StringExpAssertionBuilder(stringConstant(string))

  def fromStringVariable[T](string: T => String): StringExpAssertionBuilder[T] =
    StringExpAssertionBuilder(stringVariable(string))

  def apply[T](stringExp: StringExp[T]): StringExpAssertionBuilder[T] =
    StringExpAssertionBuilder(stringExp, new NullExp[T,Bool]())

  def apply[T](stringExp: StringExp[T], expression: ComposableBooleanExp[T]): StringExpAssertionBuilder[T] =
    new StringExpAssertionBuilder(stringExp, expression, _ and _)
}

case class StringExpAssertionBuilder[T](stringExp: StringExp[T], expression: ComposableBooleanExp[T], operator: (ComposableBooleanExp[T], ComposableBooleanExp[T]) => ComposableBooleanExp[T])
  extends AssertionBuilder[T,StringExpAssertionBuilder[T]](expression) {

  import StringExp._

  def isEqualTo(string: String): StringExpAssertionBuilder[T] =
    isEqualTo(_ => string)

  def isEqualTo(string: T => String): StringExpAssertionBuilder[T] =
    newWith(stringExp.isEqualTo(stringVariable(string)))

  def isEqualToIgnoringCase(string: String): StringExpAssertionBuilder[T] =
    isEqualToIgnoringCase(_ => string)

  def isEqualToIgnoringCase(string: T => String): StringExpAssertionBuilder[T] =
    newWith(stringExp.isEqualToIgnoringCase(stringVariable(string)))

  def startsWith(prefix: String): StringExpAssertionBuilder[T] =
    startsWith(_ => prefix)

  def startsWith(prefix: T => String): StringExpAssertionBuilder[T] =
    newWith(stringExp.startsWith(stringVariable(prefix)))

  def startsWithIgnoringCase(prefix: String): StringExpAssertionBuilder[T] =
    startsWithIgnoringCase(_ => prefix)

  def startsWithIgnoringCase(prefix: T => String): StringExpAssertionBuilder[T] =
    newWith(stringExp.startsWithIgnoringCase(stringVariable(prefix)))

  def endsWith(suffix: String): StringExpAssertionBuilder[T] =
    endsWith(_ => suffix)

  def endsWith(suffix: T => String): StringExpAssertionBuilder[T] =
    newWith(stringExp.endsWith(stringVariable(suffix)))

  def endsWithIgnoringCase(suffix: String): StringExpAssertionBuilder[T] =
    endsWithIgnoringCase(_ => suffix)

  def endsWithIgnoringCase(suffix: T => String): StringExpAssertionBuilder[T] =
    newWith(stringExp.endsWithIgnoringCase(stringVariable(suffix)))

  def contains(string: String): StringExpAssertionBuilder[T] =
    contains(_ => string)

  def contains(string: T => String): StringExpAssertionBuilder[T] =
    newWith(stringExp.contains(stringVariable(string)))

  def containsIgnoringCase(string: String): StringExpAssertionBuilder[T] =
    containsIgnoringCase(_ => string)

  def containsIgnoringCase(string: T => String): StringExpAssertionBuilder[T] =
    newWith(stringExp.containsIgnoringCase(stringVariable(string)))

  def matches(regex: String): StringExpAssertionBuilder[T] =
    matches(_ => regex)

  def matches(regex: T => String): StringExpAssertionBuilder[T] =
    newWith(stringExp.matches(stringVariable(regex)))

  def isEmail: StringExpAssertionBuilder[T] =
    matches(emailRegex)

  def isUri: StringExpAssertionBuilder[T] =
    matches(uriRegex)

  def isAlphanumeric: StringExpAssertionBuilder[T] =
    matches(alphanumericRegex)

  def isAlphabetic: StringExpAssertionBuilder[T] =
    matches(alphabeticRegex)

  def isNumber: StringExpAssertionBuilder[T] =
    matches(numberRegex)

  def isSameLengthAs(length: Int): StringExpAssertionBuilder[T] =
    isSameLengthAs(_ => length)

  def isSameLengthAs(length: T => Int): StringExpAssertionBuilder[T] =
    newWith(stringExp.isSameLengthAs(length))

  def isLongerThan(length: Int): StringExpAssertionBuilder[T] =
    isLongerThan(_ => length)

  def isLongerThan(length: T => Int): StringExpAssertionBuilder[T] =
    newWith(stringExp.isLongerThan(length))

  def isShorterThan(length: Int): StringExpAssertionBuilder[T] =
    isShorterThan(_ => length)

  def isShorterThan(length: T => Int): StringExpAssertionBuilder[T] =
    newWith(stringExp.isShorterThan(length))

  def isLongerThanOrEqualTo(length: Int): StringExpAssertionBuilder[T] =
    isLongerThanOrEqualTo(_ => length)

  def isLongerThanOrEqualTo(length: T => Int): StringExpAssertionBuilder[T] =
    newWith(stringExp.isLongerThanOrEqualTo(length))

  def isShorterThanOrEqualTo(length: Int): StringExpAssertionBuilder[T] =
    isShorterThanOrEqualTo(_ => length)

  def isShorterThanOrEqualTo(length: T => Int): StringExpAssertionBuilder[T] =
    newWith(stringExp.isShorterThanOrEqualTo(length))

  def isBlank: StringExpAssertionBuilder[T] =
    newWith(stringExp.isBlank)

  def isNotBlank: StringExpAssertionBuilder[T] =
    newWith(stringExp.isNotBlank)

  private def newWith(newExpression: ComposableBooleanExp[T]) =
    StringExpAssertionBuilder(stringExp, operator.apply(expression, newExpression))

  override def or: StringExpAssertionBuilder[T] =
    StringExpAssertionBuilder(stringExp, expression, _ or _)
}
