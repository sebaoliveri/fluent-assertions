package expression

object StringExp {

  val Blank = ""
  val emailRegex: String = "^[\\w!#$%&'*+/=?`{|}~^-]+(?:\\.[\\w!#$%&'*+/=?`{|}~^-]+)*@(?:[a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,6}$"
  val uriRegex =  "https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,4}\\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*)"
  val alphanumericRegex = "^[a-zA-Z0-9 ]+$"
  val alphabeticRegex = "^[a-zA-Z ]+$"
  val numberRegex = "^[0-9]+$"

  def stringConstant[T](string: => String): StringExp[T] =
    stringVariable(_ => string)

  def stringVariable[T](f: T => String): StringExp[T] =
    StringExp(f)
}

case class StringExp[T](func: T => String) extends AnyExp[T,String](func) {

  import StringExp._
  import QuantifiableOrderedExp._

  def isEqualToIgnoringCase(stringExp: StringExp[T]): IsEqualToExp[T,String] =
    IsEqualToExp(StringExp(func.andThen(_.toUpperCase())), StringExp(stringExp.func.andThen(_.toUpperCase())))

  def startsWith(stringExp: StringExp[T]): LogicalOperatorsExp[T,Bool] =
    IterableExp(func.andThen(_.toIterable)).containsAllInSameOrder(IterableExp(stringExp.func.andThen(_.toIterable)))

  def startsWithIgnoringCase(stringExp: StringExp[T]): LogicalOperatorsExp[T,Bool] =
    IterableExp(func.andThen(_.toUpperCase).andThen(_.toIterable))
      .containsAllInSameOrder(IterableExp(stringExp.func.andThen(_.toUpperCase).andThen(_.toIterable)))

  def endsWith(stringExp: StringExp[T]): LogicalOperatorsExp[T,Bool] =
    IterableExp(func.andThen(_.toIterable).andThen(_.reverse))
      .containsAllInSameOrder(IterableExp(stringExp.func.andThen(_.toIterable).andThen(_.reverse)))

  def endsWithIgnoringCase(stringExp: StringExp[T]): LogicalOperatorsExp[T,Bool] =
    IterableExp(func.andThen(_.toUpperCase).andThen(_.toIterable).andThen(_.reverse))
      .containsAllInSameOrder(IterableExp(stringExp.func.andThen(_.toUpperCase).andThen(_.toIterable).andThen(_.reverse)))

  def contains(stringExp: StringExp[T]): LogicalOperatorsExp[T,Bool] =
    IterableExp(func.andThen(_.toIterable)).containsSomeInSameOrder(IterableExp(stringExp.func.andThen(_.toIterable)))

  def containsIgnoringCase(stringExp: StringExp[T]): LogicalOperatorsExp[T,Bool] =
    IterableExp(func.andThen(_.toUpperCase).andThen(_.toIterable)).containsSomeInSameOrder(IterableExp(stringExp.func.andThen(_.toUpperCase).andThen(_.toIterable)))

  def matches(stringExp: StringExp[T]): MatchesExp[T] =
    MatchesExp(this, stringExp)

  def isSameLengthAs(length: Int): IsEqualToExp[T,Ordered[Int]] =
    isSameLengthAs(_ => length)

  def isSameLengthAs(length: T => Int): IsEqualToExp[T,Ordered[Int]] =
    intVariable(func.andThen(_.length)).isEqualTo(intVariable(length))

  def isLongerThan(length: Int): IsGreaterThanExp[T,Int] =
    isLongerThan(_ => length)

  def isLongerThan(length: T => Int): IsGreaterThanExp[T,Int] =
    intVariable(func.andThen(_.length)).isGreaterThan(QuantifiableExp(length))

  def isShorterThan(length: Int): IsLessThanExp[T,Int] =
    isShorterThan(_ => length)

  def isShorterThan(length: T => Int): IsLessThanExp[T,Int] =
    intVariable(func.andThen(_.length)).isLessThan(QuantifiableExp(length))

  def isLongerThanOrEqualTo(length: Int): LogicalOperatorsExp[T,Bool] =
    isLongerThanOrEqualTo(_ => length)

  def isLongerThanOrEqualTo(length: T => Int): LogicalOperatorsExp[T,Bool] =
    intVariable(func.andThen(_.length)).isGreaterThanOrEqualTo(QuantifiableExp(length))

  def isShorterThanOrEqualTo(length: Int): LogicalOperatorsExp[T,Bool] =
    isShorterThanOrEqualTo(_ => length)

  def isShorterThanOrEqualTo(length: T => Int): LogicalOperatorsExp[T,Bool] =
    intVariable(func.andThen(_.length)).isLessThanOrEqualTo(QuantifiableExp(length))

  def isBlank: IsEqualToExp[T,String] =
    IsEqualToExp(stringVariable(func.andThen(_.trim)), stringConstant(Blank))

  def isNotBlank: NotExp[T] =
    NotExp(isBlank)
}

case class MatchesExp[T](stringExp: StringExp[T], regex: StringExp[T]) extends LogicalOperatorsExp[T,Bool] {

  override def evaluate(context: T): Bool =
    Bool(stringExp.evaluate(context).matches(regex.evaluate(context)))
}
