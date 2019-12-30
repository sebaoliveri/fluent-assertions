package expression

object BoolExp {

  def boolConstant[T](bool: => Boolean): BoolExp[T] =
    boolVariable(_ => bool)

  def boolVariable[T](f: T => Boolean): BoolExp[T] =
    BoolExp(f)
}

case class BoolExp[T](func: T => Boolean) extends TypeExp[T,Boolean](func) {

  def isTrue: IsEqualToExp[T,Boolean] = IsEqualToExp(ObjectExp(func), ObjectExp((_:T) => true))

  def isFalse: NotExp[T] = NotExp(isTrue)

  override def evaluate(context: T): Boolean = func(context)
}
