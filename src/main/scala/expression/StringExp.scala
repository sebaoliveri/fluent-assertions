package expression

object StringExp {

  val Blank = ""
  val emailRegex: String = "^[\\w!#$%&'*+/=?`{|}~^-]+(?:\\.[\\w!#$%&'*+/=?`{|}~^-]+)*@(?:[a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,6}$"
  val uriRegex = "^(https?:\\/\\/)?(www\\.)?([\\w]+\\.)+[\u200C\u200B\\w]{2,63}\\/?$"
  val alphanumericRegex = "^[a-zA-Z0-9]+$"
  val alphabeticRegex = "^[a-zA-Z]+$"
  val numberRegex = "^[0-9]+$"

  def stringConstant[T](string: => String): StringExp[T] =
    stringVariable(_ => string)

  def stringVariable[T](f: T => String): StringExp[T] =
    StringExp(f)
}

case class StringExp[T](func: T => String) extends TypeExp[T,String](func) {

  import StringExp._
  import QuantifiableOrderedExp._

  def isEqualToIgnoringCase(stringExp: StringExp[T]): IsEqualToExp[T,String] =
    IsEqualToExp(StringExp(func.andThen(_.toUpperCase())), StringExp(stringExp.func.andThen(_.toUpperCase())))

  def startsWith(stringExp: StringExp[T]): StartsWithExp[T] =
    StartsWithExp(this, stringExp)

  def startsWithIgnoringCase(stringExp: StringExp[T]): StartsWithExp[T] =
    StartsWithExp(StringExp(func.andThen(_.toUpperCase)), StringExp(stringExp.func.andThen(_.toUpperCase)))

  def endsWith(stringExp: StringExp[T]): EndsWithExp[T] =
    EndsWithExp(this, stringExp)

  def endsWithIgnoringCase(stringExp: StringExp[T]): EndsWithExp[T] =
    EndsWithExp(StringExp(func.andThen(_.toUpperCase)), StringExp(stringExp.func.andThen(_.toUpperCase)))

  def contains(stringExp: StringExp[T]): ContainsStringExp[T] =
    ContainsStringExp(this, stringExp)

  def containsIgnoringCase(stringExp: StringExp[T]): ContainsStringExp[T] =
    ContainsStringExp(StringExp(func.andThen(_.toUpperCase)), StringExp(stringExp.func.andThen(_.toUpperCase)))

  def matches(stringExp: StringExp[T]): MatchesExp[T] =
    MatchesExp(this, stringExp)

  def isSameLengthAs(length: Int): IsEqualToExp[T, Ordered[Int]] =
    isSameLengthAs(_ => length)

  def isSameLengthAs(length: T => Int): IsEqualToExp[T, Ordered[Int]] =
    IsEqualToExp(intVariable(func.andThen(_.length)), intVariable(length))

  def isLongerThan(length: Int): IsGreaterThanExp[T,Int] =
    IsGreaterThanExp(intVariable(func.andThen(_.length)), QuantifiableExp(_ => length))

  def isLongerThan(length: T => Int): IsGreaterThanExp[T,Int] =
    IsGreaterThanExp(intVariable(func.andThen(_.length)), QuantifiableExp(length))

  def isShorterThan(length: Int): IsLessThanExp[T,Int] =
    IsLessThanExp(intVariable(func.andThen(_.length)), QuantifiableExp(_ => length))

  def isShorterThan(length: T => Int): IsLessThanExp[T,Int] =
    IsLessThanExp(intVariable(func.andThen(_.length)), QuantifiableExp(length))

  def isLongerThanOrEqualTo(length: Int): BooleanExp[T, Bool] =
    isLongerThanOrEqualTo({_:T => length})

  def isLongerThanOrEqualTo(length: T => Int): BooleanExp[T, Bool] =
    IsGreaterThanOrEqualToExp(intVariable(func.andThen(_.length)), QuantifiableExp(length))

  def isShorterThanOrEqualTo(length: Int): BooleanExp[T, Bool] =
    isShorterThanOrEqualTo({_:T => length})

  def isShorterThanOrEqualTo(length: T => Int): BooleanExp[T, Bool] =
    IsLessThanOrEqualToExp(intVariable(func.andThen(_.length)), QuantifiableExp(length))

  def isBlank: IsEqualToExp[T,String] =
    IsEqualToExp(stringVariable(func.andThen(_.trim)), stringConstant(Blank))

  def isNotBlank: NotExp[T] =
    NotExp(isBlank)

  override def evaluate(context: T): String =
    func(context)
}

case class StartsWithExp[T](left: StringExp[T], right: StringExp[T]) extends BooleanExp[T,Bool] {

  override def evaluate(context: T): Bool =
    Bool(left.evaluate(context).startsWith(right.evaluate(context)))
}

case class EndsWithExp[T](left: StringExp[T], right: StringExp[T]) extends BooleanExp[T,Bool] {

  override def evaluate(context: T): Bool =
    Bool(left.evaluate(context).endsWith(right.evaluate(context)))
}

case class ContainsStringExp[T](left: StringExp[T], right: StringExp[T]) extends BooleanExp[T,Bool] {

  override def evaluate(context: T): Bool =
    Bool(left.evaluate(context).contains(right.evaluate(context)))
}

case class MatchesExp[T](stringExp: StringExp[T], regex: StringExp[T]) extends BooleanExp[T,Bool] {

  override def evaluate(context: T): Bool =
    Bool(stringExp.evaluate(context).matches(regex.evaluate(context)))
}
