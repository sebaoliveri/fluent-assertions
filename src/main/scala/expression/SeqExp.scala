//package expression
//
//case class SeqExp[T,R](func: T => Seq[R]) extends TypeExp[T,Seq[R]](func) {
//  def contains(anObject: R) = SeqContainsExp(this, ObjectExp(_ => anObject))
//  def exists(predicate: R => Boolean) = SeqExistsExp(this, predicate)
//  def forall(predicate: R => Boolean) = SeqForAllExp(this, predicate)
//  //reduce?
//  override def evaluate(context: T): Seq[R] = func(context)
//}
//
//case class SeqContainsExp[T,R](seqExp: SeqExp[T,R], objectExp: ObjectExp[T,R]) extends BooleanExp[T,Bool] {
//  override def evaluate(context: T): Bool = Bool(seqExp.evaluate(context).contains(objectExp.evaluate(context)))
//}
//
//case class SeqExistsExp[T,R](seqExp: SeqExp[T,R], predicate: R => Boolean) extends BooleanExp[T,Bool] {
//  override def evaluate(context: T): Bool = Bool(seqExp.evaluate(context).exists(predicate))
//}
//
//case class SeqForAllExp[T,R](seqExp: SeqExp[T,R], predicate: R => Boolean) extends BooleanExp[T,Bool] {
//  override def evaluate(context: T): Bool = Bool(seqExp.evaluate(context).forall(predicate))
//}
