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
  def assertThat[T](int: Int): QuantifiableExpAssertionBuilder[T,Int] =
    fromIntConstant(int)
  def assertThat[T](int: T => Int)(implicit d:DummyImplicit): QuantifiableExpAssertionBuilder[T,Int] =
    fromIntVariable(int)
  def assertThat[T](double: Double): QuantifiableExpAssertionBuilder[T,Double] =
    fromDoubleConstant(double)
  def assertThat[T](double: T => Double)(implicit d1:DummyImplicit,d2:DummyImplicit): QuantifiableExpAssertionBuilder[T,Double] =
    fromDoubleVariable(double)
  def assertThat[T](long: Long): QuantifiableExpAssertionBuilder[T,Long] =
    fromLongConstant(long)
  def assertThat[T](long: T => Long)(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit): QuantifiableExpAssertionBuilder[T,Long] =
    fromLongVariable(long)
  def assertThat[T](bigDecimal: BigDecimal): QuantifiableExpAssertionBuilder[T,BigDecimal] =
    fromBigDecimalConstant(bigDecimal)
  def assertThat[T](bigDecimal: T => BigDecimal)(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit): QuantifiableExpAssertionBuilder[T,BigDecimal] =
    fromBigDecimalVariable(bigDecimal)

  // optional quantifiable
  import OptionalQuantifiableExpAssertionBuilder._
  def assertThat[T](maybeInt: Option[Int]): OptionalQuantifiableExpAssertionBuilder[T,Int] =
    fromMaybeIntConstant(maybeInt)
  def assertThat[T](maybeInt: T => Option[Int])(implicit d:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Int] =
    fromMaybeIntVariable(maybeInt)
  def assertThat[T](maybeDouble: Option[Double])(implicit d:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Double] =
    fromMaybeDoubleConstant(maybeDouble)
  def assertThat[T](maybeDouble: T => Option[Double])(implicit d1:DummyImplicit,d2:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Double] =
    fromMaybeDoubleVariable(maybeDouble)
  def assertThat[T](maybeLong: Option[Long])(implicit d1:DummyImplicit,d2:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Long] =
    fromMaybeLongConstant(maybeLong)
  def assertThat[T](maybeLong: T => Option[Long])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Long] =
    fromMaybeLongVariable(maybeLong)
  def assertThat[T](maybeBigDecimal: Option[BigDecimal])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,BigDecimal] =
    fromMaybeBigDecimalConstant(maybeBigDecimal)
  def assertThat[T](maybeBigDecimal: T => Option[BigDecimal]): OptionalQuantifiableExpAssertionBuilder[T,BigDecimal] =
    fromMaybeBigDecimalVariable(maybeBigDecimal)

  // date
  import DateExpAssertionBuilder._
  def assertThat[T](instant: Instant): DateExpAssertionBuilder[T,Instant] =
    fromInstantConstant(instant)
  def assertThat[T](zonedDateTime: ZonedDateTime): DateExpAssertionBuilder[T,ZonedDateTime] =
    fromZonedDateTimeConstant(zonedDateTime)
  def assertThat[T](localDate: LocalDate): DateExpAssertionBuilder[T,LocalDate] =
    fromLocalDateConstant(localDate)
  def assertThat[T](localDateTime: LocalDateTime): DateExpAssertionBuilder[T,LocalDateTime] =
    fromLocalDateTimeConstant(localDateTime)
  def assertThat[T](instant: T => Instant): DateExpAssertionBuilder[T,Instant] =
    fromInstantVariable(instant)
  def assertThat[T](zonedDateTime: T => ZonedDateTime)(implicit d1:DummyImplicit): DateExpAssertionBuilder[T,ZonedDateTime] =
    fromZonedDateTimeVariable(zonedDateTime)
  def assertThat[T](localDate: T => LocalDate)(implicit d1:DummyImplicit,d2:DummyImplicit): DateExpAssertionBuilder[T,LocalDate] =
    fromLocalDateVariable(localDate)
  def assertThat[T](localDateTime: T => LocalDateTime)(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit): DateExpAssertionBuilder[T,LocalDateTime] =
    fromLocalDateTimeVariable(localDateTime)

  // optional date
  import OptionalDateExpAssertionBuilder._
  def assertThat[T](instant: Option[Instant]): OptionalDateExpAssertionBuilder[T,Instant] =
    fromMaybeInstantConstant(instant)
  def assertThat[T](zonedDateTime: Option[ZonedDateTime])(implicit d1:DummyImplicit): OptionalDateExpAssertionBuilder[T,ZonedDateTime] =
    fromMaybeZonedDateTimeConstant(zonedDateTime)
  def assertThat[T](localDate: Option[LocalDate])(implicit d1:DummyImplicit,d2:DummyImplicit): OptionalDateExpAssertionBuilder[T,LocalDate] =
    fromMaybeLocalDateConstant(localDate)
  def assertThat[T](localDateTime: Option[LocalDateTime])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit): OptionalDateExpAssertionBuilder[T,LocalDateTime] =
    fromMaybeLocalDateTimeConstant(localDateTime)
  def assertThat[T](instant: T => Option[Instant]): OptionalDateExpAssertionBuilder[T,Instant] =
    fromMaybeInstantVariable(instant)
  def assertThat[T](zonedDateTime: T => Option[ZonedDateTime])(implicit d1:DummyImplicit): OptionalDateExpAssertionBuilder[T,ZonedDateTime] =
    fromMaybeZonedDateTimeVariable(zonedDateTime)
  def assertThat[T](localDate: T => Option[LocalDate])(implicit d1:DummyImplicit,d2:DummyImplicit): OptionalDateExpAssertionBuilder[T,LocalDate] =
    fromMaybeLocalDateVariable(localDate)
  def assertThat[T](localDateTime: T => Option[LocalDateTime])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit): OptionalDateExpAssertionBuilder[T,LocalDateTime] =
    fromMaybeLocalDateTimeVariable(localDateTime)
}

object Assert {
  def assert[T](expression: Expression[T,AssertionResultBehaviour[T]]) = Assert(expression)
}

case class Assert[T](expression: Expression[T,AssertionResultBehaviour[T]]) {

  private val NoContext = new Object().asInstanceOf[T]

  def expectsToBeTrue(): Unit =
    verifiedIn(NoContext).expectsToBeTrue()
  def expectsToBeFalseWith(errorMessages: String*): Unit =
    verifiedIn(NoContext).expectsToBeFalseWith(errorMessages:_*)

  def signalIfFailed(exception: List[String] => Throwable): Unit =
    verifiedIn(NoContext).signalIfFailed(exception)
  def signalFirstFailureIfFailed(exception: String => Throwable): Unit =
    verifiedIn(NoContext).signalFirstFailureIfFailed(exception)

  def verified(): AssertionResultBehaviour[T] = verifiedIn(NoContext)
  def verifiedIn(context: T): AssertionResultBehaviour[T] = expression.evaluate(context)
}
