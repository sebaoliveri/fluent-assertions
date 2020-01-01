package expression

object OptionalExp {

  def maybeStringConstant[T,R](option: Option[R]): OptionalExp[T, R] =
    maybeStringVariable(_ => option)

  def maybeStringVariable[T,R](option: T => Option[R]): OptionalExp[T, R] =
    OptionalExp(option)
}

case class OptionalExp[T,R](func: T => Option[R]) extends TypeExp[T,Option[R]](func) {

  override def evaluate(context: T): Option[R] =
    func(context)
}

case class OptionalBoolExp[T,R](constantExp: OptionalExp[T,R], expressionProvider: R => LogicalOperatorsExp[T,Bool]) extends LogicalOperatorsExp[T,Bool] {

  override def evaluate(context: T): Bool =
    constantExp.evaluate(context)
      .map(optionValue => expressionProvider(optionValue).evaluate(context))
      .getOrElse(TrueExp)
}

case class IsDefinedExp[T,R](constantExp: OptionalExp[T,R]) extends LogicalOperatorsExp[T,Bool] {

  override def evaluate(context: T): Bool =
    Bool(constantExp.evaluate(context).isDefined)
}

