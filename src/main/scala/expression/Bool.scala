package expression

trait Bool extends LogicalOperators[Bool] {

  def thenElse[T](t: => T, e: => T): T

  def not(): Bool
}

object Bool {

  def apply(boolean: Boolean): Bool =
    if (boolean) TrueExp else FalseExp
}

object TrueExp extends Bool  {

  override def thenElse[T](t: => T, e: => T): T = t

  override def and(other: => Bool): Bool = other

  override def or(other: => Bool): Bool = this

  override def not(): Bool = FalseExp

  override def ifTrue(block: => Bool): Bool = block
}

object FalseExp extends Bool {

  override def thenElse[T](t: => T, e: => T): T = e

  override def and(other: => Bool): Bool = this

  override def or(other: => Bool): Bool = other

  override def not(): Bool = TrueExp

  override def ifTrue(block: => Bool): Bool = this
}
