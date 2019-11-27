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

case class OptionalBoolExp[T,R](constantExp: OptionalExp[T,R], expressionProvider: R => BooleanExp[T,Bool]) extends BooleanExp[T,Bool] {

  override def evaluate(context: T): Bool =
    constantExp.evaluate(context)
      .map(optionValue => expressionProvider(optionValue).evaluate(context))
      .getOrElse(TrueExp)
}

case class IfDefinedExp[T,R](constantExp: OptionalExp[T,R], expression: BooleanExp[T,Bool] = new NullBooleanExp[T,Bool]()) extends BooleanExp[T,Bool] {

  override def and(anotherExpression: BooleanExp[T,Bool]): BooleanExp[T,Bool] =
    copy(expression = expression.and(anotherExpression))

  override def or(anotherExpression: BooleanExp[T,Bool]): BooleanExp[T,Bool] =
    copy(expression = expression.or(anotherExpression))

  override def evaluate(context: T): Bool =
    constantExp.evaluate(context)
      .map(_ => expression.evaluate(context))
      .getOrElse(FalseExp)
}
