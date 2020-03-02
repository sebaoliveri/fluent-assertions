package org.validify.expression

case class MapAssertionExp[P1,P2,P3](expression: Expression[P1,AssertionResultBehaviour[P2]],
                                     f: P2 => P3)
                                     extends ComposableAssertionExp[P1,P2,P3] {

  override def evaluate(context: P1): AssertionResultBehaviour[P3] =
    expression.evaluate(context).map(f)
}
