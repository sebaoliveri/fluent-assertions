package expression

object OptionalExp {

  def maybeOptionConstant[T,R](option: Option[R]): OptionalExp[T, R] =
    maybeOptionVariable(_ => option)

  def maybeOptionVariable[T,R](option: T => Option[R]): OptionalExp[T, R] =
    OptionalExp(option)
}

case class OptionalExp[T,R](func: T => Option[R]) extends AnyExp[T,Option[R]](func)

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

