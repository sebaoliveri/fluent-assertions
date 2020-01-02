package expression
//
//case class ConstantExp[R](value: R) extends Expression[_,R] {
//  override def evaluate(context: _): R = value
//}
//
//case class VariableExp[T,R](value: T => R) extends Expression[T,R] {
//  override def evaluate(context: T): R = value(context)
//}
