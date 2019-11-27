package assertion

import expression._

object OptionalStringExpAssertionBuilder {

  import OptionalExp._

  def fromMaybeStringConstant[T](maybeString: Option[String]): OptionalStringExpAssertionBuilder[T]=
    OptionalStringExpAssertionBuilder(maybeStringConstant(maybeString))

  def fromMaybeStringVariable[T](maybeString: T => Option[String]): OptionalStringExpAssertionBuilder[T] =
    OptionalStringExpAssertionBuilder(maybeStringVariable(maybeString))

  def apply[T](optionalExp: OptionalExp[T,String]): OptionalStringExpAssertionBuilder[T] =
    OptionalStringExpAssertionBuilder(optionalExp, new NullBooleanExp[T,Bool]())

  def apply[T](optionalExp: OptionalExp[T,String], expression: BooleanExp[T,Bool]): OptionalStringExpAssertionBuilder[T] =
    new OptionalStringExpAssertionBuilder(optionalExp, expression, _ and _)
}

case class OptionalStringExpAssertionBuilder[T](optionExp: OptionalExp[T,String], expression: BooleanExp[T,Bool], operator: (BooleanExp[T,Bool], BooleanExp[T,Bool]) => BooleanExp[T,Bool])
  extends BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]](expression) {

  import StringExp._

  def isDefined: StringExpAssertionBuilder[T] =
    StringExpAssertionBuilder(
      stringVariable(optionExp.func.andThen(_.get)),
      operator(expression, IfDefinedExp[T,String](optionExp)))

  def wouldBeEqualTo(string: String): OptionalStringExpAssertionBuilder[T] =
    wouldEqualTo(_ => string)

  def wouldEqualTo(string: T => String): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).isEqualTo(stringVariable(string)))

  def wouldBeEqualToIgnoringCase(string: String): OptionalStringExpAssertionBuilder[T] =
    wouldEqualToIgnoringCase(_ => string)

  def wouldEqualToIgnoringCase(string: T => String): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).isEqualToIgnoringCase(stringVariable(string)))

  def wouldStartWith(prefix: String): OptionalStringExpAssertionBuilder[T] =
    wouldStartWith(_ => prefix)

  def wouldStartWith(prefix: T => String): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).startsWith(stringVariable(prefix)))

  def wouldStartWithIgnoringCase(prefix: String): OptionalStringExpAssertionBuilder[T] =
    wouldStartWithIgnoringCase(_ => prefix)

  def wouldStartWithIgnoringCase(prefix: T => String): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).startsWithIgnoringCase(stringVariable(prefix)))

  def wouldEndWith(suffix: String): OptionalStringExpAssertionBuilder[T] =
    wouldEndWith(_ => suffix)

  def wouldEndWith(suffix: T => String): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).endsWith(stringVariable(suffix)))

  def wouldEndWithIgnoringCase(suffix: String): OptionalStringExpAssertionBuilder[T] =
    wouldEndWithIgnoringCase(_ => suffix)

  def wouldEndWithIgnoringCase(suffix: T => String): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).endsWithIgnoringCase(stringVariable(suffix)))

  def wouldContain(string: String): OptionalStringExpAssertionBuilder[T] =
    wouldContain(_ => string)

  def wouldContain(string: T => String): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).contains(stringVariable(string)))

  def wouldContainIgnoringCase(string: String): OptionalStringExpAssertionBuilder[T] =
    wouldContainIgnoringCase(_ => string)

  def wouldContainIgnoringCase(string: T => String): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).containsIgnoringCase(stringVariable(string)))

  def wouldMatch(regex: String): OptionalStringExpAssertionBuilder[T] =
    wouldMatch(_ => regex)

  def wouldMatch(regex: T => String): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).matches(stringVariable(regex)))

  def wouldBeEmail: OptionalStringExpAssertionBuilder[T] =
    wouldMatch(emailRegex)

  def wouldBeUri: OptionalStringExpAssertionBuilder[T] =
    wouldMatch(uriRegex)

  def wouldBeAlphanumeric: OptionalStringExpAssertionBuilder[T] =
    wouldMatch(alphanumericRegex)

  def wouldBeAlphabetic: OptionalStringExpAssertionBuilder[T] =
    wouldMatch(alphabeticRegex)

  def wouldBeNumber: OptionalStringExpAssertionBuilder[T] =
    wouldMatch(numberRegex)

  def wouldBeSameLengthAs(length: Int): OptionalStringExpAssertionBuilder[T] =
    wouldBeSameLengthAs(_ => length)

  def wouldBeSameLengthAs(length: T => Int): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).isSameLengthAs(length))

  def wouldBeLongerThan(length: Int): OptionalStringExpAssertionBuilder[T] =
    wouldBeLongerThan(_ => length)

  def wouldBeLongerThan(length: T => Int): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).isLongerThan(length))

  def wouldBeShorterThan(length: Int): OptionalStringExpAssertionBuilder[T] =
    wouldBeShorterThan(_ => length)

  def wouldBeShorterThan(length: T => Int): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).isShorterThan(length))

  def wouldBeLongerThanOrEqualTo(length: Int): OptionalStringExpAssertionBuilder[T] =
    wouldBeLongerThanOrEqualTo(_ => length)

  def wouldBeLongerThanOrEqualTo(length: T => Int): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).isLongerThanOrEqualTo(length))

  def wouldBeShorterThanOrEqualTo(length: Int): OptionalStringExpAssertionBuilder[T] =
    wouldBeShorterThanOrEqualTo(_ => length)

  def wouldBeShorterThanOrEqualTo(length: T => Int): OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).isShorterThanOrEqualTo(length))

  def wouldBeBlank: OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).isBlank)

  def wouldBeNotBlank: OptionalStringExpAssertionBuilder[T] =
    newWith(stringConstant(_).isNotBlank)

  override def or: OptionalStringExpAssertionBuilder[T] =
    OptionalStringExpAssertionBuilder(optionExp, expression, _ or _)

  private def newWith(newExpression: String => BooleanExp[T,Bool]) =
    OptionalStringExpAssertionBuilder(optionExp,
      operator.apply(expression, OptionalBoolExp[T,String](optionExp, newExpression)))
}
