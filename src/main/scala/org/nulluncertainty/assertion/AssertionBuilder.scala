package org.nulluncertainty.assertion

import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}

import scala.runtime.{RichDouble, RichInt, RichLong}
import org.nulluncertainty.expression._
import scala.reflect.ClassTag

object AssertionBuilder {

  import BooleanExpAssertionBuilder._
  def assertThat[T](bool: Boolean): BooleanExpAssertionBuilder[T] =
    fromBooleanConstant(bool)
  def assertThat[T](bool: T => Boolean): BooleanExpAssertionBuilder[T] =
    fromBooleanVariable(bool)

  // traversable
  import IterableExpAssertionBuilder._
  def assertThat[T,R](iterable: Iterable[R]): IterableExpAssertionBuilder[T,R] =
    fromIterableConstant(iterable)
  def assertThat[T,R](iterable: T => Iterable[R]): IterableExpAssertionBuilder[T,R] =
    fromIterableVariable(iterable)

  // maybe traversable
  import OptionalIterableExpAssertionBuilder._
  def assertThat[T,R](iterable: Option[Iterable[R]]): OptionalIterableExpAssertionBuilder[T,R] =
    fromMaybeIterableConstant(iterable)
  def assertThat[T,R](iterable: T => Option[Iterable[R]]): OptionalIterableExpAssertionBuilder[T,R] =
    fromMaybeIterableVariable(iterable)

  // string
  import StringExpAssertionBuilder._
  def assertThat[T](string: String): StringExpAssertionBuilder[T] =
    fromStringConstant(string)
  def assertThat[T](string: T => String): StringExpAssertionBuilder[T] =
    fromStringVariable(string)

  // maybe string
  import OptionalStringExpAssertionBuilder._
  def assertThat[T](maybeString: Option[String]): OptionalStringExpAssertionBuilder[T] =
    fromMaybeStringConstant(maybeString)
  def assertThat[T](maybeString: T => Option[String]): OptionalStringExpAssertionBuilder[T] =
    fromMaybeStringVariable(maybeString)

  // quantifiable
  import QuantifiableExpAssertionBuilder._
  def assertThat[T](int: Int): QuantifiableExpAssertionBuilder[T,Int] =
    fromIntConstant(int)
  def assertThat[T](int: T => Int)(implicit d:DummyImplicit): QuantifiableExpAssertionBuilder[T,Int] =
    fromIntVariable(int)
  def assertThat[T](double: Double): QuantifiableExpAssertionBuilder[T,Double] =
    fromDoubleConstant(double)
  def assertThat[T](double: T => Double)(implicit d1:DummyImplicit, d2:DummyImplicit): QuantifiableExpAssertionBuilder[T,Double] =
    fromDoubleVariable(double)
  def assertThat[T](long: Long): QuantifiableExpAssertionBuilder[T,Long] =
    fromLongConstant(long)
  def assertThat[T](long: T => Long)(implicit d1:DummyImplicit, d2:DummyImplicit, d3:DummyImplicit): QuantifiableExpAssertionBuilder[T,Long] =
    fromLongVariable(long)
  def assertThat[T](bigDecimal: BigDecimal): QuantifiableExpAssertionBuilder[T,BigDecimal] =
    fromBigDecimalConstant(bigDecimal)
  def assertThat[T](bigDecimal: T => BigDecimal)(implicit d1:DummyImplicit, d2:DummyImplicit, d3:DummyImplicit, d4:DummyImplicit): QuantifiableExpAssertionBuilder[T,BigDecimal] =
    fromBigDecimalVariable(bigDecimal)

  // maybe quantifiable
  import OptionalQuantifiableExpAssertionBuilder._
  def assertThat[T](maybeInt: Option[Int]): OptionalQuantifiableExpAssertionBuilder[T,Int] =
    fromMaybeIntConstant(maybeInt)
  def assertThat[T](maybeInt: T => Option[Int])(implicit d:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Int] =
    fromMaybeIntVariable(maybeInt)
  def assertThat[T](maybeDouble: Option[Double])(implicit d:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Double] =
    fromMaybeDoubleConstant(maybeDouble)
  def assertThat[T](maybeDouble: T => Option[Double])(implicit d1:DummyImplicit, d2:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Double] =
    fromMaybeDoubleVariable(maybeDouble)
  def assertThat[T](maybeLong: Option[Long])(implicit d1:DummyImplicit, d2:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Long] =
    fromMaybeLongConstant(maybeLong)
  def assertThat[T](maybeLong: T => Option[Long])(implicit d1:DummyImplicit, d2:DummyImplicit, d3:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Long] =
    fromMaybeLongVariable(maybeLong)
  def assertThat[T](maybeBigDecimal: Option[BigDecimal])(implicit d1:DummyImplicit, d2:DummyImplicit, d3:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,BigDecimal] =
    fromMaybeBigDecimalConstant(maybeBigDecimal)
  def assertThat[T](maybeBigDecimal: T => Option[BigDecimal]): OptionalQuantifiableExpAssertionBuilder[T,BigDecimal] =
    fromMaybeBigDecimalVariable(maybeBigDecimal)

  // date
  import TemporalExpAssertionBuilder._
  def assertThat[T](instant: Instant): TemporalExpAssertionBuilder[T,Instant] =
    fromInstantConstant(instant)
  def assertThat[T](zonedDateTime: ZonedDateTime): TemporalExpAssertionBuilder[T,ZonedDateTime] =
    fromZonedDateTimeConstant(zonedDateTime)
  def assertThat[T](localDate: LocalDate): TemporalExpAssertionBuilder[T,LocalDate] =
    fromLocalDateConstant(localDate)
  def assertThat[T](localDateTime: LocalDateTime): TemporalExpAssertionBuilder[T,LocalDateTime] =
    fromLocalDateTimeConstant(localDateTime)
  def assertThat[T](instant: T => Instant): TemporalExpAssertionBuilder[T,Instant] =
    fromInstantVariable(instant)
  def assertThat[T](zonedDateTime: T => ZonedDateTime)(implicit d1:DummyImplicit): TemporalExpAssertionBuilder[T,ZonedDateTime] =
    fromZonedDateTimeVariable(zonedDateTime)
  def assertThat[T](localDate: T => LocalDate)(implicit d1:DummyImplicit, d2:DummyImplicit): TemporalExpAssertionBuilder[T,LocalDate] =
    fromLocalDateVariable(localDate)
  def assertThat[T](localDateTime: T => LocalDateTime)(implicit d1:DummyImplicit, d2:DummyImplicit, d3:DummyImplicit): TemporalExpAssertionBuilder[T,LocalDateTime] =
    fromLocalDateTimeVariable(localDateTime)

  // maybe date
  import OptionalTemporalExpAssertionBuilder._
  def assertThat[T](instant: Option[Instant]): OptionalTemporalExpAssertionBuilder[T,Instant] =
    fromMaybeInstantConstant(instant)
  def assertThat[T](zonedDateTime: Option[ZonedDateTime])(implicit d1:DummyImplicit): OptionalTemporalExpAssertionBuilder[T,ZonedDateTime] =
    fromMaybeZonedDateTimeConstant(zonedDateTime)
  def assertThat[T](localDate: Option[LocalDate])(implicit d1:DummyImplicit, d2:DummyImplicit): OptionalTemporalExpAssertionBuilder[T,LocalDate] =
    fromMaybeLocalDateConstant(localDate)
  def assertThat[T](localDateTime: Option[LocalDateTime])(implicit d1:DummyImplicit, d2:DummyImplicit, d3:DummyImplicit): OptionalTemporalExpAssertionBuilder[T,LocalDateTime] =
    fromMaybeLocalDateTimeConstant(localDateTime)
  def assertThat[T](instant: T => Option[Instant]): OptionalTemporalExpAssertionBuilder[T,Instant] =
    fromMaybeInstantVariable(instant)
  def assertThat[T](zonedDateTime: T => Option[ZonedDateTime])(implicit d1:DummyImplicit): OptionalTemporalExpAssertionBuilder[T,ZonedDateTime] =
    fromMaybeZonedDateTimeVariable(zonedDateTime)
  def assertThat[T](localDate: T => Option[LocalDate])(implicit d1:DummyImplicit, d2:DummyImplicit): OptionalTemporalExpAssertionBuilder[T,LocalDate] =
    fromMaybeLocalDateVariable(localDate)
  def assertThat[T](localDateTime: T => Option[LocalDateTime])(implicit d1:DummyImplicit, d2:DummyImplicit, d3:DummyImplicit): OptionalTemporalExpAssertionBuilder[T,LocalDateTime] =
    fromMaybeLocalDateTimeVariable(localDateTime)
}

abstract class AssertionBuilder[T,U <: AssertionBuilder[T,U]](expression: ComposableBooleanExp[T]) {

  def or: U

  def andThat(boolean: Boolean): BooleanExpAssertionBuilder[T] = andThat(_ => boolean)
  def andThat(boolean: T => Boolean): BooleanExpAssertionBuilder[T] = BooleanExpAssertionBuilder(BooleanExp(boolean), expression, _ and _)
  def orThat(boolean: Boolean): BooleanExpAssertionBuilder[T] = orThat(_ => boolean)
  def orThat(boolean: T => Boolean): BooleanExpAssertionBuilder[T] = BooleanExpAssertionBuilder(BooleanExp(boolean), expression, _ or _)

  //TODO add option boolean

  def andThat[R](iterable: Iterable[R]): IterableExpAssertionBuilder[T, R] = andThat(_ => iterable)
  def andThat[R](iterable: T => Iterable[R]): IterableExpAssertionBuilder[T, R] = new IterableExpAssertionBuilder(IterableExp(iterable), expression, _ and _)
  def orThat[R](iterable: Iterable[R]): IterableExpAssertionBuilder[T, R] = orThat(_ => iterable)
  def orThat[R](iterable: T => Iterable[R]): IterableExpAssertionBuilder[T, R] = new IterableExpAssertionBuilder(IterableExp(iterable), expression, _ or _)

  def andThat[R](iterable: Option[Iterable[R]]): OptionalIterableExpAssertionBuilder[T, R] = andThat(_ => iterable)
  def andThat[R](iterable: T => Option[Iterable[R]]): OptionalIterableExpAssertionBuilder[T, R] = new OptionalIterableExpAssertionBuilder(OptionalExp(iterable), expression, _ and _)
  def orThat[R](iterable: Option[Iterable[R]]): OptionalIterableExpAssertionBuilder[T, R] = orThat(_ => iterable)
  def orThat[R](iterable: T => Option[Iterable[R]]): OptionalIterableExpAssertionBuilder[T, R] = new OptionalIterableExpAssertionBuilder(OptionalExp(iterable), expression, _ or _)

  def andThat(quantity: Int): QuantifiableExpAssertionBuilder[T,Int] = andThat(_ => quantity)
  def andThat(quantity: T => Int): QuantifiableExpAssertionBuilder[T,Int] = andQuantifiableExp(quantity.andThen(new RichInt(_)))
  def andThat(quantity: Long): QuantifiableExpAssertionBuilder[T,Long] = andThat(_ => quantity)
  def andThat(quantity: T => Long)(implicit d1:DummyImplicit): QuantifiableExpAssertionBuilder[T,Long] = andQuantifiableExp(quantity.andThen(new RichLong(_)))
  def andThat(quantity: Double): QuantifiableExpAssertionBuilder[T,Double] = andThat(_ => quantity)
  def andThat(quantity: T => Double)(implicit d1:DummyImplicit,d2:DummyImplicit): QuantifiableExpAssertionBuilder[T,Double] = andQuantifiableExp(quantity.andThen(new RichDouble(_)))
  def andThat(quantity: BigDecimal): QuantifiableExpAssertionBuilder[T,BigDecimal] = andThat(_ => quantity)
  def andThat(quantity: T => BigDecimal)(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit): QuantifiableExpAssertionBuilder[T,BigDecimal] = andQuantifiableExp(quantity)
  private def andQuantifiableExp[R: ClassTag](quantity: T => Ordered[R]): QuantifiableExpAssertionBuilder[T,R] = QuantifiableExpAssertionBuilder(QuantifiableOrderedExp(quantity), expression, _ and _)

  def orThat(quantity: Int): QuantifiableExpAssertionBuilder[T,Int] = orThat(_ => quantity)
  def orThat(quantity: T => Int): QuantifiableExpAssertionBuilder[T,Int] = orQuantifiableExp(quantity.andThen(new RichInt(_)))
  def orThat(quantity: Long): QuantifiableExpAssertionBuilder[T,Long] = orThat(_ => quantity)
  def orThat(quantity: T => Long)(implicit d1:DummyImplicit): QuantifiableExpAssertionBuilder[T,Long] = orQuantifiableExp(quantity.andThen(new RichLong(_)))
  def orThat(quantity: Double): QuantifiableExpAssertionBuilder[T,Double] = orThat(_ => quantity)
  def orThat(quantity: T => Double)(implicit d1:DummyImplicit,d2:DummyImplicit): QuantifiableExpAssertionBuilder[T,Double] = orQuantifiableExp(quantity.andThen(new RichDouble(_)))
  def orThat(quantity: BigDecimal): QuantifiableExpAssertionBuilder[T,BigDecimal] = orThat(_ => quantity)
  def orThat(quantity: T => BigDecimal)(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit): QuantifiableExpAssertionBuilder[T,BigDecimal] = orQuantifiableExp(quantity)
  private def orQuantifiableExp[R: ClassTag](quantity: T => Ordered[R]): QuantifiableExpAssertionBuilder[T,R] = QuantifiableExpAssertionBuilder(QuantifiableOrderedExp(quantity), expression, _ or _)

  def andThat(quantity: Option[Int]): OptionalQuantifiableExpAssertionBuilder[T,Int] = andMaybeQuantifiableExp({_:T => quantity}.andThen(_.map(new RichInt(_))))
  def andThat(quantity: T => Option[Int]): OptionalQuantifiableExpAssertionBuilder[T, Int] = andMaybeQuantifiableExp(quantity.andThen(_.map(new RichInt(_))))
  def andThat(quantity: Option[Long])(implicit d1:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Long] = andMaybeQuantifiableExp({_:T => quantity}.andThen(_.map(new RichLong(_))))
  def andThat(quantity: T => Option[Long])(implicit d1:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Long] = andMaybeQuantifiableExp(quantity.andThen(_.map(new RichLong(_))))
  def andThat(quantity: Option[Double])(implicit d1:DummyImplicit,d2:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Double] = andMaybeQuantifiableExp({_:T => quantity}.andThen(_.map(new RichDouble(_))))
  def andThat(quantity: T => Option[Double])(implicit d1:DummyImplicit,d2:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Double] = andMaybeQuantifiableExp(quantity.andThen(_.map(new RichDouble(_))))
  def andThat(quantity: Option[BigDecimal])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,BigDecimal] = andMaybeQuantifiableExp({_:T => quantity})
  def andThat(quantity: T => Option[BigDecimal])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,BigDecimal] = andMaybeQuantifiableExp(quantity)
  private def andMaybeQuantifiableExp[R](maybeQuantity: T => Option[Ordered[R]]): OptionalQuantifiableExpAssertionBuilder[T,R] = OptionalQuantifiableExpAssertionBuilder(OptionalExp(maybeQuantity), expression, _ and _)

  def orThat(quantity: Option[Int]): OptionalQuantifiableExpAssertionBuilder[T,Int] = orMaybeQuantifiableExp({_:T => quantity}.andThen(_.map(new RichInt(_))))
  def orThat(quantity: T => Option[Int]): OptionalQuantifiableExpAssertionBuilder[T, Int] = orMaybeQuantifiableExp(quantity.andThen(_.map(new RichInt(_))))
  def orThat(quantity: Option[Long])(implicit d1:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Long] = orMaybeQuantifiableExp({_:T => quantity}.andThen(_.map(new RichLong(_))))
  def orThat(quantity: T => Option[Long])(implicit d1:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Long] = orMaybeQuantifiableExp(quantity.andThen(_.map(new RichLong(_))))
  def orThat(quantity: Option[Double])(implicit d1:DummyImplicit,d2:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Double] = orMaybeQuantifiableExp({_:T => quantity}.andThen(_.map(new RichDouble(_))))
  def orThat(quantity: T => Option[Double])(implicit d1:DummyImplicit,d2:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,Double] = orMaybeQuantifiableExp(quantity.andThen(_.map(new RichDouble(_))))
  def orThat(quantity: Option[BigDecimal])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,BigDecimal] = orMaybeQuantifiableExp({_:T => quantity})
  def orThat(quantity: T => Option[BigDecimal])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit): OptionalQuantifiableExpAssertionBuilder[T,BigDecimal] = orMaybeQuantifiableExp(quantity)
  private def orMaybeQuantifiableExp[R](maybeQuantity: T => Option[Ordered[R]]): OptionalQuantifiableExpAssertionBuilder[T,R] = OptionalQuantifiableExpAssertionBuilder(OptionalExp(maybeQuantity), expression, _ or _)

  def andThat(string: String): StringExpAssertionBuilder[T] = andThat(_ => string)
  def andThat(string: T => String): StringExpAssertionBuilder[T] = StringExpAssertionBuilder(StringExp(string), expression, _ and _)
  def orThat(string: String): StringExpAssertionBuilder[T] = orThat(_ => string)
  def orThat(string: T => String): StringExpAssertionBuilder[T] = StringExpAssertionBuilder(StringExp(string), expression, _ or _)

  def andThat(string: Option[String])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit): OptionalStringExpAssertionBuilder[T] = OptionalStringExpAssertionBuilder(OptionalExp({_:T => string}), expression, _ and _)
  def andThat(string: T => Option[String])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit): OptionalStringExpAssertionBuilder[T] = OptionalStringExpAssertionBuilder(OptionalExp(string), expression, _ and _)
  def orThat(string: Option[String])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit): OptionalStringExpAssertionBuilder[T] = OptionalStringExpAssertionBuilder(OptionalExp({_:T => string}), expression, _ or _)
  def orThat(string: T => Option[String])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit): OptionalStringExpAssertionBuilder[T] = OptionalStringExpAssertionBuilder(OptionalExp(string), expression, _ or _)

  def andThat(instant: Instant): TemporalExpAssertionBuilder[T,Instant] = andThat(_ => instant)
  def andThat(instant: T => Instant): TemporalExpAssertionBuilder[T,Instant] = andDateExp(instant.andThen(OrderedInstant))
  def andThat(zonedDateTime: ZonedDateTime): TemporalExpAssertionBuilder[T,ZonedDateTime] = andThat(_ => zonedDateTime)
  def andThat(zonedDateTime: T => ZonedDateTime)(implicit d1:DummyImplicit): TemporalExpAssertionBuilder[T,ZonedDateTime] = andDateExp(zonedDateTime.andThen(OrderedZonedDateTime))
  def andThat(localDate: LocalDate): TemporalExpAssertionBuilder[T,LocalDate] = andThat(_ => localDate)
  def andThat(localDate: T => LocalDate)(implicit d1:DummyImplicit,d2:DummyImplicit): TemporalExpAssertionBuilder[T,LocalDate] = andDateExp(localDate.andThen(OrderedLocalDate))
  def andThat(localDateTime: LocalDateTime): TemporalExpAssertionBuilder[T,LocalDateTime] = andThat(_ => localDateTime)
  def andThat(localDateTime: T => LocalDateTime)(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit): TemporalExpAssertionBuilder[T,LocalDateTime] = andDateExp(localDateTime.andThen(OrderedLocalDateTime))
  private def andDateExp[R](dateExp: T => Ordered[R]): TemporalExpAssertionBuilder[T,R] = new TemporalExpAssertionBuilder(QuantifiableOrderedExp(dateExp), expression, _ and _)

  def orThat(instant: Instant): TemporalExpAssertionBuilder[T,Instant] = orThat(_ => instant)
  def orThat(instant: T => Instant): TemporalExpAssertionBuilder[T,Instant] = orDateExp(instant.andThen(OrderedInstant))
  def orThat(zonedDateTime: ZonedDateTime): TemporalExpAssertionBuilder[T,ZonedDateTime] = orThat(_ => zonedDateTime)
  def orThat(zonedDateTime: T => ZonedDateTime)(implicit d1:DummyImplicit): TemporalExpAssertionBuilder[T,ZonedDateTime] = orDateExp(zonedDateTime.andThen(OrderedZonedDateTime))
  def orThat(localDate: LocalDate): TemporalExpAssertionBuilder[T,LocalDate] = orThat(_ => localDate)
  def orThat(localDate: T => LocalDate)(implicit d1:DummyImplicit,d2:DummyImplicit): TemporalExpAssertionBuilder[T,LocalDate] = orDateExp(localDate.andThen(OrderedLocalDate))
  def orThat(localDateTime: LocalDateTime): TemporalExpAssertionBuilder[T,LocalDateTime] = orThat(_ => localDateTime)
  def orThat(localDateTime: T => LocalDateTime)(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit): TemporalExpAssertionBuilder[T,LocalDateTime] = orDateExp(localDateTime.andThen(OrderedLocalDateTime))
  private def orDateExp[R](dateExp: T => Ordered[R]): TemporalExpAssertionBuilder[T,R] = new TemporalExpAssertionBuilder(QuantifiableOrderedExp(dateExp), expression, _ or _)

  def andThat(instant: Option[Instant])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit): OptionalTemporalExpAssertionBuilder[T,Instant] = andThatMaybeDateExp({ _:T => instant}.andThen(_.map(OrderedInstant)))
  def andThat(instant: T => Option[Instant])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit): OptionalTemporalExpAssertionBuilder[T,Instant] = andThatMaybeDateExp(instant.andThen(_.map(OrderedInstant)))
  def andThat(zonedDateTime: Option[ZonedDateTime])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit): OptionalTemporalExpAssertionBuilder[T,ZonedDateTime] = andThatMaybeDateExp({ _:T => zonedDateTime}.andThen(_.map(OrderedZonedDateTime)))
  def andThat(zonedDateTime: T => Option[ZonedDateTime])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit): OptionalTemporalExpAssertionBuilder[T,ZonedDateTime] = andThatMaybeDateExp(zonedDateTime.andThen(_.map(OrderedZonedDateTime)))
  def andThat(localDate: Option[LocalDate])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit,d7:DummyImplicit): OptionalTemporalExpAssertionBuilder[T,LocalDate] = andThatMaybeDateExp({ _:T => localDate}.andThen(_.map(OrderedLocalDate)))
  def andThat(localDate: T => Option[LocalDate])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit,d7:DummyImplicit): OptionalTemporalExpAssertionBuilder[T,LocalDate] = andThatMaybeDateExp(localDate.andThen(_.map(OrderedLocalDate)))
  def andThat(localDateTime: Option[LocalDateTime])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit,d7:DummyImplicit,d8:DummyImplicit): OptionalTemporalExpAssertionBuilder[T,LocalDateTime] = andThatMaybeDateExp({ _:T => localDateTime}.andThen(_.map(OrderedLocalDateTime)))
  def andThat(localDateTime: T => Option[LocalDateTime])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit,d7:DummyImplicit,d8:DummyImplicit): OptionalTemporalExpAssertionBuilder[T,LocalDateTime] = andThatMaybeDateExp(localDateTime.andThen(_.map(OrderedLocalDateTime)))
  private def andThatMaybeDateExp[R](maybeOrdered: T => Option[Ordered[R]]): OptionalTemporalExpAssertionBuilder[T,R] = new OptionalTemporalExpAssertionBuilder(OptionalExp(maybeOrdered), expression, _ and _)

  def orThat(instant: Option[Instant])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit): OptionalTemporalExpAssertionBuilder[T,Instant] = orThatMaybeDateExp({ _:T => instant}.andThen(_.map(OrderedInstant)))
  def orThat(instant: T => Option[Instant])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit): OptionalTemporalExpAssertionBuilder[T,Instant] = orThatMaybeDateExp(instant.andThen(_.map(OrderedInstant)))
  def orThat(zonedDateTime: Option[ZonedDateTime])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit): OptionalTemporalExpAssertionBuilder[T,ZonedDateTime] = orThatMaybeDateExp({ _:T => zonedDateTime}.andThen(_.map(OrderedZonedDateTime)))
  def orThat(zonedDateTime: T => Option[ZonedDateTime])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit): OptionalTemporalExpAssertionBuilder[T,ZonedDateTime] = orThatMaybeDateExp(zonedDateTime.andThen(_.map(OrderedZonedDateTime)))
  def orThat(localDate: Option[LocalDate])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit,d7:DummyImplicit): OptionalTemporalExpAssertionBuilder[T,LocalDate] = orThatMaybeDateExp({ _:T => localDate}.andThen(_.map(OrderedLocalDate)))
  def orThat(localDate: T => Option[LocalDate])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit,d7:DummyImplicit): OptionalTemporalExpAssertionBuilder[T,LocalDate] = orThatMaybeDateExp(localDate.andThen(_.map(OrderedLocalDate)))
  def orThat(localDateTime: Option[LocalDateTime])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit,d7:DummyImplicit,d8:DummyImplicit): OptionalTemporalExpAssertionBuilder[T,LocalDateTime] = orThatMaybeDateExp({ _:T => localDateTime}.andThen(_.map(OrderedLocalDateTime)))
  def orThat(localDateTime: T => Option[LocalDateTime])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit,d7:DummyImplicit,d8:DummyImplicit): OptionalTemporalExpAssertionBuilder[T,LocalDateTime] = orThatMaybeDateExp(localDateTime.andThen(_.map(OrderedLocalDateTime)))
  private def orThatMaybeDateExp[R](maybeOrdered: T => Option[Ordered[R]]): OptionalTemporalExpAssertionBuilder[T,R] = new OptionalTemporalExpAssertionBuilder(OptionalExp(maybeOrdered), expression, _ or _)

  def otherwise[E](error: E): AssertionExp[T,E] = AssertionExp(expression, { _:T => error})
  def otherwise[E](error: T => E): AssertionExp[T,E] = AssertionExp(expression, error)
}
