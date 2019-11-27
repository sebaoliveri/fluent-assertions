package expression

case class ObjectExp[T,R <: Any](func: T => R) extends TypeExp[T,R](func) {

  def isTrue(predicate: R => Boolean): PredicateExp[T,R] =
    PredicateExp(this, predicate)

  override def evaluate(context: T): R =
    func(context)
}

case class PredicateExp[T,R](objectExp: ObjectExp[T,R], predicate: R => Boolean) extends BooleanExp[T,Bool] {

  override def evaluate(context: T): Bool =
    Bool(predicate.apply(objectExp.evaluate(context)))
}
