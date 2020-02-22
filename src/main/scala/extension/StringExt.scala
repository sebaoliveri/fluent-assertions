package extension

import expression.{Bool, LogicalOperatorsExp}

object StringExt {

  import expression.StringExp._

  implicit class StringExtensions(string: String) {

    def isEqualToExp(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).isEqualTo(stringConstant(anotherString))

    def isEqualToIgnoringCaseExp(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).isEqualToIgnoringCase(stringConstant(anotherString))

    def startsWithExp(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).startsWith(stringConstant(anotherString))

    def startsWithIgnoringCaseExp(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).startsWithIgnoringCase(stringConstant(anotherString))

    def endsWithExp(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).endsWith(stringConstant(anotherString))

    def endsWithIgnoringCaseExp(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).endsWithIgnoringCase(stringConstant(anotherString))

    def containsExp(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).contains(stringConstant(anotherString))

    def containsIgnoringCaseExp(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).containsIgnoringCase(stringConstant(anotherString))

    def matchesRegexExp(regex: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).matches(stringConstant(regex))

    def isEmail: LogicalOperatorsExp[Unit, Bool] = matchesRegexExp(emailRegex)

    def isUri: LogicalOperatorsExp[Unit, Bool] = matchesRegexExp(uriRegex)

    def isAlphanumeric: LogicalOperatorsExp[Unit, Bool] = matchesRegexExp(alphanumericRegex)

    def isAlphabetic: LogicalOperatorsExp[Unit, Bool] = matchesRegexExp(alphabeticRegex)

    def isNumber: LogicalOperatorsExp[Unit, Bool] = matchesRegexExp(numberRegex)

    def isSameLengthAsExp(length: Int): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).isSameLengthAs(length)

    def isLongerThanExp(length: Int): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).isLongerThan(length)

    def isShorterThanExp(length: Int): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).isShorterThan(length)

    def isLongerThanOrEqualToExp(length: Int): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).isLongerThanOrEqualTo(length)

    def isShorterThanOrEqualToExp(length: Int): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).isShorterThanOrEqualTo(length)

    def isBlankExp: LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).isBlank

    def isNotBlankExp: LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).isNotBlank
  }
}
