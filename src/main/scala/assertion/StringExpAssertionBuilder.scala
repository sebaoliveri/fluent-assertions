package assertion

import expression._

object StringExpAssertionBuilder {
  import StringExp._
  def fromStringConstant[T](string: String): StringExpAssertionBuilder[T]= StringExpAssertionBuilder(stringConstant(string))
  def fromStringVariable[T](string: T => String): StringExpAssertionBuilder[T] = StringExpAssertionBuilder(stringVariable(string))
  def apply[T](stringExp: StringExp[T]): StringExpAssertionBuilder[T] = new StringExpAssertionBuilder(stringExp, new NullBooleanExp[T,Bool](), _ and _)
}

case class StringExpAssertionBuilder[T](stringExp: StringExp[T], expression: BooleanExp[T,Bool], operator: (BooleanExp[T,Bool], BooleanExp[T,Bool]) => BooleanExp[T,Bool]) extends AssertionBuilder[T] {
  import StringExp._
  private def newBoolExp(anExpression: BooleanExp[T,Bool]): BoolExpAssertionBuilder[T, StringExpAssertionBuilder[T]] =
    BoolExpAssertionBuilder(operator(expression, anExpression), (newExpression,newOperator) => StringExpAssertionBuilder(stringExp, newExpression, newOperator))
  def isEqualTo(string: String): BoolExpAssertionBuilder[T, StringExpAssertionBuilder[T]] = isEqualTo(_ => string)
  def isEqualTo(string: T => String): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = newBoolExp(stringExp.isEqualTo(stringVariable(string)))
  def isEqualToIgnoringCase(string: String): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = isEqualToIgnoringCase(_ => string)
  def isEqualToIgnoringCase(string: T => String): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = newBoolExp(stringExp.isEqualToIgnoringCase(stringVariable(string)))
  def startsWith(prefix: String): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = startsWith(_ => prefix)
  def startsWith(prefix: T => String): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = newBoolExp(stringExp.startsWith(stringVariable(prefix)))
  def startsWithIgnoringCase(prefix: String): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = startsWithIgnoringCase(_ => prefix)
  def startsWithIgnoringCase(prefix: T => String): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = newBoolExp(stringExp.startsWithIgnoringCase(stringVariable(prefix)))
  def endsWith(suffix: String): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = endsWith(_ => suffix)
  def endsWith(suffix: T => String): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = newBoolExp(stringExp.endsWith(stringVariable(suffix)))
  def endsWithIgnoringCase(suffix: String): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = endsWithIgnoringCase(_ => suffix)
  def endsWithIgnoringCase(suffix: T => String): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = newBoolExp(stringExp.endsWithIgnoringCase(stringVariable(suffix)))
  def contains(string: String): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = contains(_ => string)
  def contains(string: T => String): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = newBoolExp(stringExp.contains(stringVariable(string)))
  def containsIgnoringCase(string: String): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = containsIgnoringCase(_ => string)
  def containsIgnoringCase(string: T => String): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = newBoolExp(stringExp.containsIgnoringCase(stringVariable(string)))
  def matches(regex: String): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = matches(_ => regex)
  def matches(regex: T => String): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = newBoolExp(stringExp.matches(stringVariable(regex)))
  def isEmail: BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = matches(emailRegex)
  def isUri: BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = matches(uriRegex)
  def isAlphanumeric: BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = matches(alphanumericRegex)
  def isAlphabetic: BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = matches(alphabeticRegex)
  def isNumber: BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = matches(numberRegex)
  def isSameLengthAs(length: Int): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = isSameLengthAs(_ => length)
  def isSameLengthAs(length: T => Int): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = newBoolExp(stringExp.isSameLengthAs(length))
  def isLongerThan(length: Int): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = isLongerThan(_ => length)
  def isLongerThan(length: T => Int): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = newBoolExp(stringExp.isLongerThan(length))
  def isShorterThan(length: Int): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = isShorterThan(_ => length)
  def isShorterThan(length: T => Int): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = newBoolExp(stringExp.isShorterThan(length))
  def isLongerThanOrEqualTo(length: Int): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = isLongerThanOrEqualTo(_ => length)
  def isLongerThanOrEqualTo(length: T => Int): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = newBoolExp(stringExp.isLongerThanOrEqualTo(length))
  def isShorterThanOrEqualTo(length: Int): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] =  isShorterThanOrEqualTo(_ => length)
  def isShorterThanOrEqualTo(length: T => Int): BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] =  newBoolExp(stringExp.isShorterThanOrEqualTo(length))
  def isBlank: BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = newBoolExp(stringExp.isBlank)
  def isNotBlank: BoolExpAssertionBuilder[T,StringExpAssertionBuilder[T]] = newBoolExp(stringExp.isNotBlank)
}
