package extension

import expression.{AssertionResultBehaviour, Bool, ConditionalAssertionExp, LogicalOperatorsExp}

object BooleanExpExt {

  implicit class BooleanExpExtensions[T](booleanExp: LogicalOperatorsExp[T, Bool]) {

    def thenElse(anAssertion: LogicalOperatorsExp[T,AssertionResultBehaviour[T]],
                 anotherAssertion: LogicalOperatorsExp[T,AssertionResultBehaviour[T]]): ConditionalAssertionExp[T] =
      ConditionalAssertionExp(booleanExp, anAssertion, anotherAssertion)
  }
}
