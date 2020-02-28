package org.validify.extension

import org.validify.expression.BoolExpBehaviour

object BooleanExt {

  import org.validify.expression.BooleanExp._

  implicit class BooleanExtensions(boolean: Boolean) {

    def isTrueExp: BoolExpBehaviour[Unit] = boolConstant(boolean).isTrue

    def isFalseExp: BoolExpBehaviour[Unit] = boolConstant(boolean).isFalse
  }
}
