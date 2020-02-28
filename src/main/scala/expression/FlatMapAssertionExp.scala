package expression

case class FlatMapAssertionExp[P1,P2,P3](expression: LogicalOperatorsExp[P1,AssertionResultBehaviour[P2]],
                                    f: P2 => LogicalOperatorsExp[P2,AssertionResultBehaviour[P3]])
                                    extends AssertionExpBehaviour[P1,P2,P3] {

  override def evaluate(context: P1): AssertionResultBehaviour[P3] =
    expression.evaluate(context).flatMap(p2 => f(p2).evaluate(p2))
}
