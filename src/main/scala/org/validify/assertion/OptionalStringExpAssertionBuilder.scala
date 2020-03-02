package org.validify.assertion

import org.validify.expression._

object OptionalStringExpAssertionBuilder {

  import OptionalExp._

  def fromMaybeStringConstant[T](maybeString: Option[String]): OptionalStringExpAssertionBuilder[T]=
    OptionalStringExpAssertionBuilder(maybeOptionConstant(maybeString))

  def fromMaybeStringVariable[T](maybeString: T => Option[String]): OptionalStringExpAssertionBuilder[T] =
    OptionalStringExpAssertionBuilder(maybeOptionVariable(maybeString))

  def apply[T](optionalExp: OptionalExp[T,String]): OptionalStringExpAssertionBuilder[T] =
    OptionalStringExpAssertionBuilder(optionalExp, new NullExp[T,Bool]())

  def apply[T](optionalExp: OptionalExp[T,String], expression: ComposableBooleanExp[T]): OptionalStringExpAssertionBuilder[T] =
    new OptionalStringExpAssertionBuilder(optionalExp, expression, _ and _)
}

case class OptionalStringExpAssertionBuilder[T](optionExp: OptionalExp[T,String], expression: ComposableBooleanExp[T], operator: (ComposableBooleanExp[T], ComposableBooleanExp[T]) => ComposableBooleanExp[T])
  extends AssertionBuilder[T,OptionalStringExpAssertionBuilder[T]](expression) {

  import StringExp._

  def isDefined: OptionalStringExpAssertionBuilder[T] =
    OptionalStringExpAssertionBuilder(optionExp, operator(expression, IsDefinedExp[T,String](optionExp)))

  def isEqualTo(string: String): OptionalStringExpAssertionBuilder[T] =
    isEqualTo(_ => string)

  def isEqualTo(string: T => String): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).isEqualTo(stringVariable(string)))

  def isEqualToIgnoringCase(string: String): OptionalStringExpAssertionBuilder[T] =
    isEqualToIgnoringCase(_ => string)

  def isEqualToIgnoringCase(string: T => String): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).isEqualToIgnoringCase(stringVariable(string)))

  def startsWith(prefix: String): OptionalStringExpAssertionBuilder[T] =
    startsWith(_ => prefix)

  def startsWith(prefix: T => String): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).startsWith(stringVariable(prefix)))

  def startsWithIgnoringCase(prefix: String): OptionalStringExpAssertionBuilder[T] =
    startsWithIgnoringCase(_ => prefix)

  def startsWithIgnoringCase(prefix: T => String): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).startsWithIgnoringCase(stringVariable(prefix)))

  def endsWith(suffix: String): OptionalStringExpAssertionBuilder[T] =
    endsWith(_ => suffix)

  def endsWith(suffix: T => String): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).endsWith(stringVariable(suffix)))

  def endsWithIgnoringCase(suffix: String): OptionalStringExpAssertionBuilder[T] =
    endsWithIgnoringCase(_ => suffix)

  def endsWithIgnoringCase(suffix: T => String): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).endsWithIgnoringCase(stringVariable(suffix)))

  def contains(string: String): OptionalStringExpAssertionBuilder[T] =
    contains(_ => string)

  def contains(string: T => String): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).contains(stringVariable(string)))

  def containsIgnoringCase(string: String): OptionalStringExpAssertionBuilder[T] =
    containsIgnoringCase(_ => string)

  def containsIgnoringCase(string: T => String): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).containsIgnoringCase(stringVariable(string)))

  def matches(regex: String): OptionalStringExpAssertionBuilder[T] =
    matches(_ => regex)

  def matches(regex: T => String): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).matches(stringVariable(regex)))

  def isEmail: OptionalStringExpAssertionBuilder[T] =
    matches(emailRegex)

  def isUri: OptionalStringExpAssertionBuilder[T] =
    matches(uriRegex)

  def isAlphanumeric: OptionalStringExpAssertionBuilder[T] =
    matches(alphanumericRegex)

  def isAlphabetic: OptionalStringExpAssertionBuilder[T] =
    matches(alphabeticRegex)

  def isNumber: OptionalStringExpAssertionBuilder[T] =
    matches(numberRegex)

  def isSameLengthAs(length: Int): OptionalStringExpAssertionBuilder[T] =
    isSameLengthAs(_ => length)

  def isSameLengthAs(length: T => Int): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).isSameLengthAs(length))

  def isLongerThan(length: Int): OptionalStringExpAssertionBuilder[T] =
    isLongerThan(_ => length)

  def isLongerThan(length: T => Int): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).isLongerThan(length))

  def isShorterThan(length: Int): OptionalStringExpAssertionBuilder[T] =
    isShorterThan(_ => length)

  def isShorterThan(length: T => Int): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).isShorterThan(length))

  def isLongerThanOrEqualTo(length: Int): OptionalStringExpAssertionBuilder[T] =
    isLongerThanOrEqualTo(_ => length)

  def isLongerThanOrEqualTo(length: T => Int): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).isLongerThanOrEqualTo(length))

  def isShorterThanOrEqualTo(length: Int): OptionalStringExpAssertionBuilder[T] =
    isShorterThanOrEqualTo(_ => length)

  def isShorterThanOrEqualTo(length: T => Int): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).isShorterThanOrEqualTo(length))

  def isBlank: OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).isBlank)

  def isNotBlank: OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).isNotBlank)

  private def newWith(newExpression: String => ComposableBooleanExp[T]): OptionalStringExpAssertionBuilder[T] =
    OptionalStringExpAssertionBuilder(optionExp,
      operator.apply(expression, OptionalBoolExp[T,String](optionExp, newExpression)))

  override def or: OptionalStringExpAssertionBuilder[T] =
    OptionalStringExpAssertionBuilder(optionExp, expression, _ or _)
}
