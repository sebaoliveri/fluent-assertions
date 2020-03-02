package org.nulluncertainty.expression

trait Expression[T,R] {

  def evaluate(context: T): R
}
