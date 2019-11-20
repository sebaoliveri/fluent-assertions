package assertion

import expression._

object OptionalStringExpAssertionBuilder {
  import OptionalExp._
  def fromMaybeStringConstant[T](maybeString: Option[String]): OptionalStringExpAssertionBuilder[T]= OptionalStringExpAssertionBuilder(maybeStringConstant(maybeString))
  def fromMaybeStringVariable[T](maybeString: T => Option[String]): OptionalStringExpAssertionBuilder[T] = OptionalStringExpAssertionBuilder(maybeStringVariable(maybeString))
  def apply[T](optionalExp: OptionalExp[T,String]): OptionalStringExpAssertionBuilder[T] = new OptionalStringExpAssertionBuilder(optionalExp, new NullBooleanExp[T,Bool](), _ and _)
}

case class OptionalStringExpAssertionBuilder[T](optionExp: OptionalExp[T,String], expression: BooleanExp[T,Bool], operator: (BooleanExp[T,Bool], BooleanExp[T,Bool]) => BooleanExp[T,Bool]) extends AssertionBuilder[T] {
  import StringExp._
  private def newBoolExp(anExpression: BooleanExp[T,Bool]): BoolExpAssertionBuilder[T, OptionalStringExpAssertionBuilder[T]] =
    BoolExpAssertionBuilder(operator(expression, anExpression), (newExpression,newOperator) => OptionalStringExpAssertionBuilder(optionExp, newExpression, newOperator))
  def isDefined: BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] =
    BoolExpAssertionBuilder(
      IfDefinedExp[T,String](optionExp),
      (newExpression:BooleanExp[T,Bool], newOperator:((BooleanExp[T,Bool],BooleanExp[T,Bool])=>BooleanExp[T,Bool])) =>
        StringExpAssertionBuilder(stringVariable(optionExp.func.andThen(_.get)), newExpression, newOperator))
  def wouldEqualTo(string: String): BoolExpAssertionBuilder[T, OptionalStringExpAssertionBuilder[T]] = wouldEqualTo(_ => string)
  def wouldEqualTo(string: T => String): BoolExpAssertionBuilder[T, OptionalStringExpAssertionBuilder[T]] = newBoolExp(OptionalBoolExp[T,String](optionExp, stringConstant(_).isEqualTo(stringVariable(string))))
  def wouldEqualToIgnoringCase(string: String): BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = wouldEqualToIgnoringCase(_ => string)
  def wouldEqualToIgnoringCase(string: T => String): BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = newBoolExp(OptionalBoolExp[T,String](optionExp, stringConstant(_).isEqualToIgnoringCase(stringVariable(string))))
  def wouldStartWith(prefix: String): BoolExpAssertionBuilder[T, OptionalStringExpAssertionBuilder[T]] = wouldStartWith(_ => prefix)
  def wouldStartWith(prefix: T => String): BoolExpAssertionBuilder[T, OptionalStringExpAssertionBuilder[T]] = newBoolExp(OptionalBoolExp[T,String](optionExp, stringConstant(_).startsWith(stringVariable(prefix))))
  def wouldStartWithIgnoringCase(prefix: String): BoolExpAssertionBuilder[T, OptionalStringExpAssertionBuilder[T]] = wouldStartWithIgnoringCase(_ => prefix)
  def wouldStartWithIgnoringCase(prefix: T => String): BoolExpAssertionBuilder[T, OptionalStringExpAssertionBuilder[T]] = newBoolExp(OptionalBoolExp[T,String](optionExp, stringConstant(_).startsWithIgnoringCase(stringVariable(prefix))))
  def wouldEndWith(suffix: String): BoolExpAssertionBuilder[T, OptionalStringExpAssertionBuilder[T]] = wouldEndWith(_ => suffix)
  def wouldEndWith(suffix: T => String): BoolExpAssertionBuilder[T, OptionalStringExpAssertionBuilder[T]] = newBoolExp(OptionalBoolExp[T,String](optionExp, stringConstant(_).endsWith(stringVariable(suffix))))
  def wouldEndWithIgnoringCase(suffix: String): BoolExpAssertionBuilder[T, OptionalStringExpAssertionBuilder[T]] = wouldEndWithIgnoringCase(_ => suffix)
  def wouldEndWithIgnoringCase(suffix: T => String): BoolExpAssertionBuilder[T, OptionalStringExpAssertionBuilder[T]] = newBoolExp(OptionalBoolExp[T,String](optionExp, stringConstant(_).endsWithIgnoringCase(stringVariable(suffix))))
  def wouldContain(string: String): BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = wouldContain(_ => string)
  def wouldContain(string: T => String): BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = newBoolExp(OptionalBoolExp[T,String](optionExp, stringConstant(_).contains(stringVariable(string))))
  def wouldContainIgnoringCase(string: String): BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = wouldContainIgnoringCase(_ => string)
  def wouldContainIgnoringCase(string: T => String): BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = newBoolExp(OptionalBoolExp[T,String](optionExp, stringConstant(_).containsIgnoringCase(stringVariable(string))))
  def wouldMatch(regex: String): BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = wouldMatch(_ => regex)
  def wouldMatch(regex: T => String): BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = newBoolExp(OptionalBoolExp[T,String](optionExp, stringConstant(_).matches(stringVariable(regex))))
  def wouldBeEmail: BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = wouldMatch(emailRegex)
  def wouldBeUri: BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = wouldMatch(uriRegex)
  def wouldBeAlphanumeric: BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = wouldMatch(alphanumericRegex)
  def wouldBeAlphabetic: BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = wouldMatch(alphabeticRegex)
  def wouldBeNumber: BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = wouldMatch(numberRegex)
  def wouldBeSameLengthAs(length: Int): BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = wouldBeSameLengthAs(_ => length)
  def wouldBeSameLengthAs(length: T => Int): BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = newBoolExp(OptionalBoolExp[T,String](optionExp, stringConstant(_).isSameLengthAs(length)))
  def wouldBeLongerThan(length: Int): BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = wouldBeLongerThan(_ => length)
  def wouldBeLongerThan(length: T => Int): BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = newBoolExp(OptionalBoolExp[T,String](optionExp, stringConstant(_).isLongerThan(length)))
  def wouldBeShorterThan(length: Int): BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = wouldBeShorterThan(_ => length)
  def wouldBeShorterThan(length: T => Int): BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = newBoolExp(OptionalBoolExp[T,String](optionExp, stringConstant(_).isShorterThan(length)))
  def wouldBeLongerThanOrEqualTo(length: Int): BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = wouldBeLongerThanOrEqualTo(_ => length)
  def wouldBeLongerThanOrEqualTo(length: T => Int): BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = newBoolExp(OptionalBoolExp[T,String](optionExp, stringConstant(_).isLongerThanOrEqualTo(length)))
  def wouldBeShorterThanOrEqualTo(length: Int): BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = wouldBeShorterThanOrEqualTo(_ => length)
  def wouldBeShorterThanOrEqualTo(length: T => Int): BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = newBoolExp(OptionalBoolExp[T,String](optionExp, stringConstant(_).isShorterThanOrEqualTo(length)))
  def wouldBeBlank: BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = newBoolExp(OptionalBoolExp[T,String](optionExp, stringConstant(_).isBlank))
  def wouldBeNotBlank: BoolExpAssertionBuilder[T,OptionalStringExpAssertionBuilder[T]] = newBoolExp(OptionalBoolExp[T,String](optionExp, stringConstant(_).isNotBlank))
}

