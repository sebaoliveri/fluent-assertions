package extension

import expression.{Bool, LogicalOperatorsExp}

object StringExt {

  import expression.StringExp._

  implicit class StringExtensions(string: String) {

    def equalsTo(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).isEqualTo(stringConstant(anotherString))

    def equalsToIgnoringCase(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).isEqualToIgnoringCase(stringConstant(anotherString))

    def startsWithPrefix(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).startsWith(stringConstant(anotherString))

    def startsWithPrefixIgnoringCase(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).startsWithIgnoringCase(stringConstant(anotherString))

    def endsWithSuffix(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).endsWith(stringConstant(anotherString))

    def endsWithSuffixIgnoringCase(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).endsWithIgnoringCase(stringConstant(anotherString))

    def containsString(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).contains(stringConstant(anotherString))

    def containsStringIgnoringCase(anotherString: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).containsIgnoringCase(stringConstant(anotherString))

    def matchesRegex(regex: String): LogicalOperatorsExp[Unit,Bool] =
      stringConstant(string).matches(stringConstant(regex))

    def isEmail: LogicalOperatorsExp[Unit, Bool] = matchesRegex(emailRegex)

    def isUri: LogicalOperatorsExp[Unit, Bool] = matchesRegex(uriRegex)

    def isAlphanumeric: LogicalOperatorsExp[Unit, Bool] = matchesRegex(alphanumericRegex)

    def isAlphabetic: LogicalOperatorsExp[Unit, Bool] = matchesRegex(alphabeticRegex)

    def isNumber: LogicalOperatorsExp[Unit, Bool] = matchesRegex(numberRegex)

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
