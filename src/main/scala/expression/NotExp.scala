package expression

case class NotExp[T](expression: Expression[T,Bool]) extends BooleanExp[T,Bool] {
  override def evaluate(context: T): Bool = expression.evaluate(context).not()
}
