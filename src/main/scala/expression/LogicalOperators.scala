package expression

trait LogicalOperators[X] {

  def and(other: => X): X

  def or(other: => X): X

  def ifTrue[Z](block: => Z): Z
}
