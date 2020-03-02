package org.nulluncertainty.expression

trait LogicalOperators[X] {

  def and(other: => X): X

  def or(other: => X): X

  def ifTrue(block: => X): X

  def ifFalse(block: => X): X
}
