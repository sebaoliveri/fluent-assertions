package expression

object ObjectExp {

  def objectConstant[T,R <: Any](anObject: => R): ObjectExp[T,R] =
    objectVariable(_ => anObject.asInstanceOf[R])

  def objectVariable[T,R <: Any](anObject: T => R): ObjectExp[T,R] =
    ObjectExp(anObject)
}

case class ObjectExp[T,R <: Any](func: T => R) extends TypeExp[T,R](func) {

  override def evaluate(context: T): R =
    func(context)
}
