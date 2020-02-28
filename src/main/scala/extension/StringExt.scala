package extension

import expression.{Bool, BoolExpBehaviour, LogicalOperatorsExp}

object StringExt {

  import expression.StringExp._

  implicit class StringExtensions(string: String) {

    def isEqualToExp(anotherString: String): BoolExpBehaviour[Unit] =
      stringConstant(string).isEqualTo(stringConstant(anotherString))

    def isEqualToIgnoringCaseExp(anotherString: String): BoolExpBehaviour[Unit] =
      stringConstant(string).isEqualToIgnoringCase(stringConstant(anotherString))

    def startsWithExp(anotherString: String): BoolExpBehaviour[Unit] =
      stringConstant(string).startsWith(stringConstant(anotherString))

    def startsWithIgnoringCaseExp(anotherString: String): BoolExpBehaviour[Unit] =
      stringConstant(string).startsWithIgnoringCase(stringConstant(anotherString))

    def endsWithExp(anotherString: String): BoolExpBehaviour[Unit] =
      stringConstant(string).endsWith(stringConstant(anotherString))

    def endsWithIgnoringCaseExp(anotherString: String): BoolExpBehaviour[Unit] =
      stringConstant(string).endsWithIgnoringCase(stringConstant(anotherString))

    def containsExp(anotherString: String): BoolExpBehaviour[Unit] =
      stringConstant(string).contains(stringConstant(anotherString))

    def containsIgnoringCaseExp(anotherString: String): BoolExpBehaviour[Unit] =
      stringConstant(string).containsIgnoringCase(stringConstant(anotherString))

    def matchesRegexExp(regex: String): BoolExpBehaviour[Unit] =
      stringConstant(string).matches(stringConstant(regex))

    def isEmail: BoolExpBehaviour[Unit] = matchesRegexExp(emailRegex)

    def isUri: BoolExpBehaviour[Unit] = matchesRegexExp(uriRegex)

    def isAlphanumeric: BoolExpBehaviour[Unit] = matchesRegexExp(alphanumericRegex)

    def isAlphabetic: BoolExpBehaviour[Unit] = matchesRegexExp(alphabeticRegex)

    def isNumber: BoolExpBehaviour[Unit] = matchesRegexExp(numberRegex)

    def isSameLengthAsExp(length: Int): BoolExpBehaviour[Unit] =
      stringConstant(string).isSameLengthAs(length)

    def isLongerThanExp(length: Int): BoolExpBehaviour[Unit] =
      stringConstant(string).isLongerThan(length)

    def isShorterThanExp(length: Int): BoolExpBehaviour[Unit] =
      stringConstant(string).isShorterThan(length)

    def isLongerThanOrEqualToExp(length: Int): BoolExpBehaviour[Unit] =
      stringConstant(string).isLongerThanOrEqualTo(length)

    def isShorterThanOrEqualToExp(length: Int): BoolExpBehaviour[Unit] =
      stringConstant(string).isShorterThanOrEqualTo(length)

    def isBlankExp: BoolExpBehaviour[Unit] =
      stringConstant(string).isBlank

    def isNotBlankExp: BoolExpBehaviour[Unit] =
      stringConstant(string).isNotBlank
  }
}
