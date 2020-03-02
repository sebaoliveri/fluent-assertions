package org.nulluncertainty.extension

import org.nulluncertainty.expression.ComposableBooleanExp

object BooleanExt {

  import org.nulluncertainty.expression.BooleanExp._

  implicit class BooleanExtensions(boolean: Boolean) {

    def isTrueExp: ComposableBooleanExp[Unit] = boolConstant(boolean).isTrue

    def isFalseExp: ComposableBooleanExp[Unit] = boolConstant(boolean).isFalse
  }
}
