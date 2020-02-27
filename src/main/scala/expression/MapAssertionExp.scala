package expression

case class MapAssertionExp[P1,P2,P3](expression: LogicalOperatorsExp[P1,AssertionResultBehaviour[P2]],
                                     f: P2 => P3) extends LogicalOperatorsExp[P1,AssertionResultBehaviour[P3]] {

  override def evaluate(context: P1): AssertionResultBehaviour[P3] =
    expression.evaluate(context).map(f)

  def map[P4](f: P3 => P4): MapAssertionExp[P1,P3,P4] =
    MapAssertionExp(this, f)

  def in(context: P1): AssertionResultBehaviour[P3] = this.evaluate(context)
}
