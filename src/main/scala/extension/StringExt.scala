package extension

import expression.{Bool, LogicalOperatorsExp}

object StringExt {

  import expression.StringExp._

  implicit class StringExtensions(string: String) {

    def isEqualToIgnoringCase(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).isEqualToIgnoringCase(stringConstant(anotherString))

    def startsWith(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).startsWith(stringConstant(anotherString))

    def startsWithIgnoringCase(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).startsWithIgnoringCase(stringConstant(anotherString))

    def endsWith(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).endsWith(stringConstant(anotherString))

    def endsWithIgnoringCase(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).endsWithIgnoringCase(stringConstant(anotherString))

    def contains(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).contains(stringConstant(anotherString))

    def containsIgnoringCase(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).containsIgnoringCase(stringConstant(anotherString))

    def matches(regex: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).matches(stringConstant(regex))

    def isEmail: LogicalOperatorsExp[Unit, Bool] = matches(emailRegex)

    def isUri: LogicalOperatorsExp[Unit, Bool] = matches(uriRegex)

    def isAlphanumeric: LogicalOperatorsExp[Unit, Bool] = matches(alphanumericRegex)

    def isAlphabetic: LogicalOperatorsExp[Unit, Bool] = matches(alphabeticRegex)

    def isNumber: LogicalOperatorsExp[Unit, Bool] = matches(numberRegex)

    def isSameLengthAs(length: Int): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).isSameLengthAs(length)

    def isLongerThan(length: Int): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).isLongerThan(length)

    def isShorterThan(length: Int): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).isShorterThan(length)

    def isLongerThanOrEqualTo(length: Int): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).isLongerThanOrEqualTo(length)

    def isShorterThanOrEqualTo(length: Int): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).isShorterThanOrEqualTo(length)

    def isBlank: LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).isBlank

    def isNotBlank: LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).isNotBlank
  }
}
