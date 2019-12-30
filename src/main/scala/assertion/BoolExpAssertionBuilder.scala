package assertion

import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}
import scala.runtime.{RichDouble, RichInt, RichLong}
import expression._

abstract class BoolExpAssertionBuilder[T,U <: BoolExpAssertionBuilder[T,U]](expression: BooleanExp[T,Bool]) {

  def or: U

  //TODO add boolean expressions to concat

  def andThat[R](iterable: collection.immutable.Iterable[R]): IterableExpAssertionBuilder[T, R] = andThat(_ => iterable)
  def andThat[R](iterable: T => collection.immutable.Iterable[R]): IterableExpAssertionBuilder[T, R] = new IterableExpAssertionBuilder(IterableExp(iterable), expression, _ and _)
  def orThat[R](iterable: collection.immutable.Iterable[R]): IterableExpAssertionBuilder[T, R] = orThat(_ => iterable)
  def orThat[R](iterable: T => collection.immutable.Iterable[R]): IterableExpAssertionBuilder[T, R] = new IterableExpAssertionBuilder(IterableExp(iterable), expression, _ or _)

  def andThat(quantity: Int): QuantifiableExpAssertionBuilder[T,Int] = andThat(_ => quantity)
  def andThat(quantity: T => Int): QuantifiableExpAssertionBuilder[T,Int] = andQuantifiableExp(quantity.andThen(new RichInt(_)))
  def andThat(quantity: Long): QuantifiableExpAssertionBuilder[T,Long] = andThat(_ => quantity)
  def andThat(quantity: T => Long)(implicit d1:DummyImplicit): QuantifiableExpAssertionBuilder[T,Long] = andQuantifiableExp(quantity.andThen(new RichLong(_)))
  def andThat(quantity: Double): QuantifiableExpAssertionBuilder[T,Double] = andThat(_ => quantity)
  def andThat(quantity: T => Double)(implicit d1:DummyImplicit,d2:DummyImplicit): QuantifiableExpAssertionBuilder[T,Double] = andQuantifiableExp(quantity.andThen(new RichDouble(_)))
  def andThat(quantity: BigDecimal): QuantifiableExpAssertionBuilder[T,BigDecimal] = andThat(_ => quantity)
  def andThat(quantity: T => BigDecimal)(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit): QuantifiableExpAssertionBuilder[T,BigDecimal] = andQuantifiableExp(quantity)
  private def andQuantifiableExp[R](quantity: T => Ordered[R]): QuantifiableExpAssertionBuilder[T,R] = QuantifiableExpAssertionBuilder(QuantifiableOrderedExp(quantity), expression, _ and _)

  def orThat(quantity: Int): QuantifiableExpAssertionBuilder[T,Int] = orThat(_ => quantity)
  def orThat(quantity: T => Int): QuantifiableExpAssertionBuilder[T,Int] = orQuantifiableExp(quantity.andThen(new RichInt(_)))
  def orThat(quantity: Long): QuantifiableExpAssertionBuilder[T,Long] = orThat(_ => quantity)
  def orThat(quantity: T => Long)(implicit d1:DummyImplicit): QuantifiableExpAssertionBuilder[T,Long] = orQuantifiableExp(quantity.andThen(new RichLong(_)))
  def orThat(quantity: Double): QuantifiableExpAssertionBuilder[T,Double] = orThat(_ => quantity)
  def orThat(quantity: T => Double)(implicit d1:DummyImplicit,d2:DummyImplicit): QuantifiableExpAssertionBuilder[T,Double] = orQuantifiableExp(quantity.andThen(new RichDouble(_)))
  def orThat(quantity: BigDecimal): QuantifiableExpAssertionBuilder[T,BigDecimal] = orThat(_ => quantity)
  def orThat(quantity: T => BigDecimal)(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit): QuantifiableExpAssertionBuilder[T,BigDecimal] = orQuantifiableExp(quantity)
  private def orQuantifiableExp[R](quantity: T => Ordered[R]): QuantifiableExpAssertionBuilder[T,R] = QuantifiableExpAssertionBuilder(QuantifiableOrderedExp(quantity), expression, _ or _)

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

  def andThat(instant: Instant): DateExpAssertionBuilder[T,Instant] = andThat(_ => instant)
  def andThat(instant: T => Instant): DateExpAssertionBuilder[T,Instant] = andDateExp(instant.andThen(OrderedInstant))
  def andThat(zonedDateTime: ZonedDateTime): DateExpAssertionBuilder[T,ZonedDateTime] = andThat(_ => zonedDateTime)
  def andThat(zonedDateTime: T => ZonedDateTime)(implicit d1:DummyImplicit): DateExpAssertionBuilder[T,ZonedDateTime] = andDateExp(zonedDateTime.andThen(OrderedZonedDateTime))
  def andThat(localDate: LocalDate): DateExpAssertionBuilder[T,LocalDate] = andThat(_ => localDate)
  def andThat(localDate: T => LocalDate)(implicit d1:DummyImplicit,d2:DummyImplicit): DateExpAssertionBuilder[T,LocalDate] = andDateExp(localDate.andThen(OrderedLocalDate))
  def andThat(localDateTime: LocalDateTime): DateExpAssertionBuilder[T,LocalDateTime] = andThat(_ => localDateTime)
  def andThat(localDateTime: T => LocalDateTime)(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit): DateExpAssertionBuilder[T,LocalDateTime] = andDateExp(localDateTime.andThen(OrderedLocalDateTime))
  private def andDateExp[R](dateExp: T => Ordered[R]): DateExpAssertionBuilder[T,R] = new DateExpAssertionBuilder(QuantifiableOrderedExp(dateExp), expression, _ and _)

  def orThat(instant: Instant): DateExpAssertionBuilder[T,Instant] = orThat(_ => instant)
  def orThat(instant: T => Instant): DateExpAssertionBuilder[T,Instant] = orDateExp(instant.andThen(OrderedInstant))
  def orThat(zonedDateTime: ZonedDateTime): DateExpAssertionBuilder[T,ZonedDateTime] = orThat(_ => zonedDateTime)
  def orThat(zonedDateTime: T => ZonedDateTime)(implicit d1:DummyImplicit): DateExpAssertionBuilder[T,ZonedDateTime] = orDateExp(zonedDateTime.andThen(OrderedZonedDateTime))
  def orThat(localDate: LocalDate): DateExpAssertionBuilder[T,LocalDate] = orThat(_ => localDate)
  def orThat(localDate: T => LocalDate)(implicit d1:DummyImplicit,d2:DummyImplicit): DateExpAssertionBuilder[T,LocalDate] = orDateExp(localDate.andThen(OrderedLocalDate))
  def orThat(localDateTime: LocalDateTime): DateExpAssertionBuilder[T,LocalDateTime] = orThat(_ => localDateTime)
  def orThat(localDateTime: T => LocalDateTime)(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit): DateExpAssertionBuilder[T,LocalDateTime] = orDateExp(localDateTime.andThen(OrderedLocalDateTime))
  private def orDateExp[R](dateExp: T => Ordered[R]): DateExpAssertionBuilder[T,R] = new DateExpAssertionBuilder(QuantifiableOrderedExp(dateExp), expression, _ or _)

  def andThat(instant: Option[Instant])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit): OptionalDateExpAssertionBuilder[T,Instant] = andThatMaybeDateExp({_:T => instant}.andThen(_.map(OrderedInstant)))
  def andThat(instant: T => Option[Instant])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit): OptionalDateExpAssertionBuilder[T,Instant] = andThatMaybeDateExp(instant.andThen(_.map(OrderedInstant)))
  def andThat(zonedDateTime: Option[ZonedDateTime])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit): OptionalDateExpAssertionBuilder[T,ZonedDateTime] = andThatMaybeDateExp({_:T => zonedDateTime}.andThen(_.map(OrderedZonedDateTime)))
  def andThat(zonedDateTime: T => Option[ZonedDateTime])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit): OptionalDateExpAssertionBuilder[T,ZonedDateTime] = andThatMaybeDateExp(zonedDateTime.andThen(_.map(OrderedZonedDateTime)))
  def andThat(localDate: Option[LocalDate])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit,d7:DummyImplicit): OptionalDateExpAssertionBuilder[T,LocalDate] = andThatMaybeDateExp({_:T => localDate}.andThen(_.map(OrderedLocalDate)))
  def andThat(localDate: T => Option[LocalDate])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit,d7:DummyImplicit): OptionalDateExpAssertionBuilder[T,LocalDate] = andThatMaybeDateExp(localDate.andThen(_.map(OrderedLocalDate)))
  def andThat(localDateTime: Option[LocalDateTime])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit,d7:DummyImplicit,d8:DummyImplicit): OptionalDateExpAssertionBuilder[T,LocalDateTime] = andThatMaybeDateExp({_:T => localDateTime}.andThen(_.map(OrderedLocalDateTime)))
  def andThat(localDateTime: T => Option[LocalDateTime])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit,d7:DummyImplicit,d8:DummyImplicit): OptionalDateExpAssertionBuilder[T,LocalDateTime] = andThatMaybeDateExp(localDateTime.andThen(_.map(OrderedLocalDateTime)))
  private def andThatMaybeDateExp[R](maybeOrdered: T => Option[Ordered[R]]): OptionalDateExpAssertionBuilder[T,R] = new OptionalDateExpAssertionBuilder(OptionalExp(maybeOrdered), expression, _ and _)

  def orThat(instant: Option[Instant])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit): OptionalDateExpAssertionBuilder[T,Instant] = orThatMaybeDateExp({_:T => instant}.andThen(_.map(OrderedInstant)))
  def orThat(instant: T => Option[Instant])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit): OptionalDateExpAssertionBuilder[T,Instant] = orThatMaybeDateExp(instant.andThen(_.map(OrderedInstant)))
  def orThat(zonedDateTime: Option[ZonedDateTime])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit): OptionalDateExpAssertionBuilder[T,ZonedDateTime] = orThatMaybeDateExp({_:T => zonedDateTime}.andThen(_.map(OrderedZonedDateTime)))
  def orThat(zonedDateTime: T => Option[ZonedDateTime])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit): OptionalDateExpAssertionBuilder[T,ZonedDateTime] = orThatMaybeDateExp(zonedDateTime.andThen(_.map(OrderedZonedDateTime)))
  def orThat(localDate: Option[LocalDate])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit,d7:DummyImplicit): OptionalDateExpAssertionBuilder[T,LocalDate] = orThatMaybeDateExp({_:T => localDate}.andThen(_.map(OrderedLocalDate)))
  def orThat(localDate: T => Option[LocalDate])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit,d7:DummyImplicit): OptionalDateExpAssertionBuilder[T,LocalDate] = orThatMaybeDateExp(localDate.andThen(_.map(OrderedLocalDate)))
  def orThat(localDateTime: Option[LocalDateTime])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit,d7:DummyImplicit,d8:DummyImplicit): OptionalDateExpAssertionBuilder[T,LocalDateTime] = orThatMaybeDateExp({_:T => localDateTime}.andThen(_.map(OrderedLocalDateTime)))
  def orThat(localDateTime: T => Option[LocalDateTime])(implicit d1:DummyImplicit,d2:DummyImplicit,d3:DummyImplicit,d4:DummyImplicit,d5:DummyImplicit,d6:DummyImplicit,d7:DummyImplicit,d8:DummyImplicit): OptionalDateExpAssertionBuilder[T,LocalDateTime] = orThatMaybeDateExp(localDateTime.andThen(_.map(OrderedLocalDateTime)))
  private def orThatMaybeDateExp[R](maybeOrdered: T => Option[Ordered[R]]): OptionalDateExpAssertionBuilder[T,R] = new OptionalDateExpAssertionBuilder(OptionalExp(maybeOrdered), expression, _ or _)

  def otherwise(errorMsg: String): AssertionExp[T] = AssertionExp(expression, { _:T => errorMsg})
  def otherwise(errorMsg: T => String): AssertionExp[T] = AssertionExp(expression, errorMsg)
}