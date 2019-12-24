package assertion

import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}
import expression._

object Assertion {

  // string
  import StringExpAssertionBuilder._
  def that[T](string: String): StringExpAssertionBuilder[T] =
    fromStringConstant(string)
  def that[T](string: T => String): StringExpAssertionBuilder[T] =
    fromStringVariable(string)

  // optional string
  import OptionalStringExpAssertionBuilder._
  def that[T](maybeString: Option[String]): OptionalStringExpAssertionBuilder[T] =
    fromMaybeStringConstant(maybeString)
  def that[T](maybeString: T => Option[String]): OptionalStringExpAssertionBuilder[T] =
    fromMaybeStringVariable(maybeString)

  // object
  import ObjectExpAssertionBuilder._
  def thatFor[T,R](anObject: T): ObjectExpAssertionBuilder[T,R] = fromObjectConstant(anObject)
  def thatFor[T,R](anObject: T => R): ObjectExpAssertionBuilder[T,R] = fromObjectVariable(anObject)

  // quantifiable
  import QuantifiableExpAssertionBuilder._
  def that[T](int: Int): QuantifiableExpAssertionBuilder[T,Int] =
    fromIntConstant(int)
  def that[T](int: T => Int)(implicit d:DummyImplicit): QuantifiableExpAssertionBuilder[T,Int] =
    fromIntVariable(int)
  def that[T](double: Double): QuantifiableExpAssertionBuilder[T,Double] =
    fromDoubleConstant(double)
  def that[T](double: T => Double)(implicit d1:DummyImplicit, d2:DummyImplicit): QuantifiableExpAssertionBuilder[T,Double] =
    fromDoubleVariable(double)
  def that[T](long: Long): QuantifiableExpAssertionBuilder[T,Long] =
    fromLongConstant(long)
  def that[T](long: T => Long)(implicit d1:DummyImplicit, d2:DummyImplicit, d3:DummyImplicit): QuantifiableExpAssertionBuilder[T,Long] =
    fromLongVariable(long)
  def that[T](bigDecimal: BigDecimal): QuantifiableExpAssertionBuilder[T,BigDecimal] =
    fromBigDecimalConstant(bigDecimal)
  def that[T](bigDecimal: T => BigDecimal)(implicit d1:DummyImplicit, d2:DummyImplicit, d3:DummyImplicit, d4:DummyImplicit): QuantifiableExpAssertionBuilder[T,BigDecimal] =
    fromBigDecimalVariable(bigDecimal)

  // optional quantifiable
  import OptionalQuantifiableExpAssertionBuilder._
  def that[T](maybeInt: Option[Int]): OptionalQuantifiableExpAssertionBuilder[T,Int] =
    fromMaybeIntConstant(maybeInt)
  def that[T](maybeInt: T => Option[Int])(implicit d:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Int] =
    fromMaybeIntVariable(maybeInt)
  def that[T](maybeDouble: Option[Double])(implicit d:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Double] =
    fromMaybeDoubleConstant(maybeDouble)
  def that[T](maybeDouble: T => Option[Double])(implicit d1:DummyImplicit, d2:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Double] =
    fromMaybeDoubleVariable(maybeDouble)
  def that[T](maybeLong: Option[Long])(implicit d1:DummyImplicit, d2:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Long] =
    fromMaybeLongConstant(maybeLong)
  def that[T](maybeLong: T => Option[Long])(implicit d1:DummyImplicit, d2:DummyImplicit, d3:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Long] =
    fromMaybeLongVariable(maybeLong)
  def that[T](maybeBigDecimal: Option[BigDecimal])(implicit d1:DummyImplicit, d2:DummyImplicit, d3:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,BigDecimal] =
    fromMaybeBigDecimalConstant(maybeBigDecimal)
  def that[T](maybeBigDecimal: T => Option[BigDecimal]): OptionalQuantifiableExpAssertionBuilder[T,BigDecimal] =
    fromMaybeBigDecimalVariable(maybeBigDecimal)

  // date
  import DateExpAssertionBuilder._
  def that[T](instant: Instant): DateExpAssertionBuilder[T,Instant] =
    fromInstantConstant(instant)
  def that[T](zonedDateTime: ZonedDateTime): DateExpAssertionBuilder[T,ZonedDateTime] =
    fromZonedDateTimeConstant(zonedDateTime)
  def that[T](localDate: LocalDate): DateExpAssertionBuilder[T,LocalDate] =
    fromLocalDateConstant(localDate)
  def that[T](localDateTime: LocalDateTime): DateExpAssertionBuilder[T,LocalDateTime] =
    fromLocalDateTimeConstant(localDateTime)
  def that[T](instant: T => Instant): DateExpAssertionBuilder[T,Instant] =
    fromInstantVariable(instant)
  def that[T](zonedDateTime: T => ZonedDateTime)(implicit d1:DummyImplicit): DateExpAssertionBuilder[T,ZonedDateTime] =
    fromZonedDateTimeVariable(zonedDateTime)
  def that[T](localDate: T => LocalDate)(implicit d1:DummyImplicit, d2:DummyImplicit): DateExpAssertionBuilder[T,LocalDate] =
    fromLocalDateVariable(localDate)
  def that[T](localDateTime: T => LocalDateTime)(implicit d1:DummyImplicit, d2:DummyImplicit, d3:DummyImplicit): DateExpAssertionBuilder[T,LocalDateTime] =
    fromLocalDateTimeVariable(localDateTime)

  // optional date
  import OptionalDateExpAssertionBuilder._
  def that[T](instant: Option[Instant]): OptionalDateExpAssertionBuilder[T,Instant] =
    fromMaybeInstantConstant(instant)
  def that[T](zonedDateTime: Option[ZonedDateTime])(implicit d1:DummyImplicit): OptionalDateExpAssertionBuilder[T,ZonedDateTime] =
    fromMaybeZonedDateTimeConstant(zonedDateTime)
  def that[T](localDate: Option[LocalDate])(implicit d1:DummyImplicit, d2:DummyImplicit): OptionalDateExpAssertionBuilder[T,LocalDate] =
    fromMaybeLocalDateConstant(localDate)
  def that[T](localDateTime: Option[LocalDateTime])(implicit d1:DummyImplicit, d2:DummyImplicit, d3:DummyImplicit): OptionalDateExpAssertionBuilder[T,LocalDateTime] =
    fromMaybeLocalDateTimeConstant(localDateTime)
  def that[T](instant: T => Option[Instant]): OptionalDateExpAssertionBuilder[T,Instant] =
    fromMaybeInstantVariable(instant)
  def that[T](zonedDateTime: T => Option[ZonedDateTime])(implicit d1:DummyImplicit): OptionalDateExpAssertionBuilder[T,ZonedDateTime] =
    fromMaybeZonedDateTimeVariable(zonedDateTime)
  def that[T](localDate: T => Option[LocalDate])(implicit d1:DummyImplicit, d2:DummyImplicit): OptionalDateExpAssertionBuilder[T,LocalDate] =
    fromMaybeLocalDateVariable(localDate)
  def that[T](localDateTime: T => Option[LocalDateTime])(implicit d1:DummyImplicit, d2:DummyImplicit, d3:DummyImplicit): OptionalDateExpAssertionBuilder[T,LocalDateTime] =
    fromMaybeLocalDateTimeVariable(localDateTime)
}

object Assert {
  def assert[T](expression: Expression[T,AssertionResultBehaviour[T]]) = Assert(expression)
}

case class Assert[T](expression: Expression[T,AssertionResultBehaviour[T]]) {

  private val NoContext = new Object().asInstanceOf[T]

  def expectsToBeTrue(): Unit =
    in(NoContext).expectsToBeTrue()
  def expectsToBeFalseWith(errorMessages: String*): Unit =
    in(NoContext).expectsToBeFalseWith(errorMessages:_*)

  def signalIfFailed(exception: List[String] => Throwable): Unit =
    in(NoContext).signalIfFailed(exception)

  def in(context: T): AssertionResultBehaviour[T] = expression.evaluate(context)
  def matches[R](partialFunction: PartialFunction[AssertionResultBehaviour[_], R]): R =
    in(NoContext).matches(partialFunction)
}
