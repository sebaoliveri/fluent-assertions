package expression

trait Expression[T,R] {
  def evaluate(context: T): R
}
