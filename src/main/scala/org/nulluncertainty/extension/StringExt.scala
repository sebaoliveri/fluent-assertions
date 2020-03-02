package org.nulluncertainty.extension

import org.nulluncertainty.expression.ComposableBooleanExp

object StringExt {

  import org.nulluncertainty.expression.StringExp._

  implicit class StringExtensions(string: String) {

    def isEqualToExp(anotherString: String): ComposableBooleanExp[Unit] =
      stringConstant(string).isEqualTo(stringConstant(anotherString))

    def isEqualToIgnoringCaseExp(anotherString: String): ComposableBooleanExp[Unit] =
      stringConstant(string).isEqualToIgnoringCase(stringConstant(anotherString))

    def startsWithExp(anotherString: String): ComposableBooleanExp[Unit] =
      stringConstant(string).startsWith(stringConstant(anotherString))

    def startsWithIgnoringCaseExp(anotherString: String): ComposableBooleanExp[Unit] =
      stringConstant(string).startsWithIgnoringCase(stringConstant(anotherString))

    def endsWithExp(anotherString: String): ComposableBooleanExp[Unit] =
      stringConstant(string).endsWith(stringConstant(anotherString))

    def endsWithIgnoringCaseExp(anotherString: String): ComposableBooleanExp[Unit] =
      stringConstant(string).endsWithIgnoringCase(stringConstant(anotherString))

    def containsExp(anotherString: String): ComposableBooleanExp[Unit] =
      stringConstant(string).contains(stringConstant(anotherString))

    def containsIgnoringCaseExp(anotherString: String): ComposableBooleanExp[Unit] =
      stringConstant(string).containsIgnoringCase(stringConstant(anotherString))

    def matchesRegexExp(regex: String): ComposableBooleanExp[Unit] =
      stringConstant(string).matches(stringConstant(regex))

    def isEmail: ComposableBooleanExp[Unit] = matchesRegexExp(emailRegex)

    def isUri: ComposableBooleanExp[Unit] = matchesRegexExp(uriRegex)

    def isAlphanumeric: ComposableBooleanExp[Unit] = matchesRegexExp(alphanumericRegex)

    def isAlphabetic: ComposableBooleanExp[Unit] = matchesRegexExp(alphabeticRegex)

    def isNumber: ComposableBooleanExp[Unit] = matchesRegexExp(numberRegex)

    def isSameLengthAsExp(length: Int): ComposableBooleanExp[Unit] =
      stringConstant(string).isSameLengthAs(length)

    def isLongerThanExp(length: Int): ComposableBooleanExp[Unit] =
      stringConstant(string).isLongerThan(length)

    def isShorterThanExp(length: Int): ComposableBooleanExp[Unit] =
      stringConstant(string).isShorterThan(length)

    def isLongerThanOrEqualToExp(length: Int): ComposableBooleanExp[Unit] =
      stringConstant(string).isLongerThanOrEqualTo(length)

    def isShorterThanOrEqualToExp(length: Int): ComposableBooleanExp[Unit] =
      stringConstant(string).isShorterThanOrEqualTo(length)

    def isBlankExp: ComposableBooleanExp[Unit] =
      stringConstant(string).isBlank

    def isNotBlankExp: ComposableBooleanExp[Unit] =
      stringConstant(string).isNotBlank
  }
}
