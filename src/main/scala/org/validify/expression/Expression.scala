package org.validify.expression

trait Expression[T,R] {

  def evaluate(context: T): R
}
