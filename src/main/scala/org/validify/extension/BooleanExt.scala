package org.validify.extension

import org.validify.expression.ComposableBooleanExp

object BooleanExt {

  import org.validify.expression.BooleanExp._

  implicit class BooleanExtensions(boolean: Boolean) {

    def isTrueExp: ComposableBooleanExp[Unit] = boolConstant(boolean).isTrue

    def isFalseExp: ComposableBooleanExp[Unit] = boolConstant(boolean).isFalse
  }
}
