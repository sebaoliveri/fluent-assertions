package expression

abstract class TypeExp[T,R](func: T => R) extends Expression[T,R] {

  def isEqualTo(another: TypeExp[T,R]): IsEqualToExp[T,R] =
    IsEqualToExp(this, another)
}

case class IsEqualToExp[T,R](left: TypeExp[T,R], right: TypeExp[T,R]) extends LogicalOperatorsExp[T,Bool] {

  override def evaluate(context: T): Bool =
    Bool(left.evaluate(context) == right.evaluate(context))
}
