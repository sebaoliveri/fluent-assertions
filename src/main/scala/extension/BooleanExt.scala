package extension

import expression.BoolExpBehaviour

object BooleanExt {

  import expression.BooleanExp._

  implicit class BooleanExtensions(boolean: Boolean) {

    def isTrueExp: BoolExpBehaviour[Unit] = boolConstant(boolean).isTrue

    def isFalseExp: BoolExpBehaviour[Unit] = boolConstant(boolean).isFalse
  }
}
