package org.nulluncertainty


import org.nulluncertainty.assertion.AssertionBuilder._
import org.scalatest.{FlatSpec, Matchers}

class OptionalStringAssertionSpec extends FlatSpec with Matchers {

  case class Customer(name: Option[String], email: Option[String], homepage: Option[String], age: Option[String])

  it should "is defined" in {
    assertThat(Some("sebastian")).isDefined.otherwise("No way it fails...").expectsToBeTrue()
    assertThat(Some("sebastian")).isDefined.startsWith("seba").otherwise("No way it fails...").expectsToBeTrue()
    assertThat(Some("sebastian")).isDefined.startsWith("eba").otherwise("Boom!").expectsToBeFalseWith("Boom!")
    assertThat(None:Option[String]).isDefined.otherwise("Boom!").expectsToBeFalseWith("Boom!")
  }

  it should "equalTo constant" in {
    assertThat(None:Option[String]).isEqualTo("Sebastian").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("Sebastian")).isEqualTo("Sebastian").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("Sebastian")).isEqualTo("sebastian").otherwise("Sebastian is not equal to sebastian")
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
    assertThat(Some("Sebastian")).isEqualTo(" sebastian").otherwise("Sebastian is not equal to sebastian")
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
    assertThat(Some("Sebastian")).isEqualTo("sebastian ").otherwise("Sebastian is not equal to sebastian")
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
    assertThat(Some(" Sebastian")).isEqualTo("sebastian").otherwise("Sebastian is not equal to sebastian")
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
    assertThat(Some("Sebastian ")).isEqualTo("sebastian").otherwise("Sebastian is not equal to sebastian")
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
  }

  it should "equalTo variable" in {
    val customer = Customer(Some("Sebastian"), None, None, None)
    assertThat({ aCustomer:Customer => aCustomer.name}).isEqualTo("Sebastian").otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    assertThat({ aCustomer:Customer => aCustomer.name}).isEqualTo("sebastian").otherwise("Sebastian is not equal to sebastian")
      .evaluate(customer)
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
  }

  it should "equalToIgnoringCase constant" in {
    assertThat(None:Option[String]).isEqualToIgnoringCase("Sebastian").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("Sebastian")).isEqualToIgnoringCase("Sebastian").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("sebastian")).isEqualToIgnoringCase("SEBASTIAN").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("SEBASTIAN")).isEqualToIgnoringCase("sebastian").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some(" Sebastian")).isEqualToIgnoringCase("sebastian").otherwise(" Sebastian is not equal to sebastian")
      .expectsToBeFalseWith(" Sebastian is not equal to sebastian")
    assertThat(Some("Sebastian ")).isEqualToIgnoringCase("sebastian").otherwise("Sebastian  is not equal to sebastian")
      .expectsToBeFalseWith("Sebastian  is not equal to sebastian")
    assertThat(Some("Sebastian")).isEqualToIgnoringCase(" Sebastian").otherwise("Sebastian is not equal to  sebastian")
      .expectsToBeFalseWith("Sebastian is not equal to  sebastian")
    assertThat(Some("Sebastian")).isEqualToIgnoringCase("Sebastian ").otherwise("Sebastian is not equal to sebastian ")
      .expectsToBeFalseWith("Sebastian is not equal to sebastian ")
  }

  it should "equalToIgnoringCase variable" in {
    val customer = Customer(Some("sebastian"), None, None, None)
    assertThat({ aCustomer:Customer => aCustomer.name}).isEqualToIgnoringCase("SEBASTIAN").otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    assertThat({ aCustomer:Customer => aCustomer.name}).isEqualToIgnoringCase("SEBASTIAN ").otherwise("Sebastian is not equal to sebastian ")
      .evaluate(customer)
      .expectsToBeFalseWith("Sebastian is not equal to sebastian ")
  }

  it should "startsWith constant" in {
    assertThat(None:Option[String]).startsWith("Sebas").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("Sebastian")).startsWith("Sebas").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("Sebastian")).startsWith("sebas").otherwise("Sebastian does not start with sebas")
      .expectsToBeFalseWith("Sebastian does not start with sebas")
    assertThat(Some("Sebastian")).startsWith(" Sebas").otherwise("Sebastian does not start with  Sebas")
      .expectsToBeFalseWith("Sebastian does not start with  Sebas")
    assertThat(Some("Sebastian")).startsWith("Sebas ").otherwise("Sebastian does not start with Sebas ")
      .expectsToBeFalseWith("Sebastian does not start with Sebas ")
  }

  it should "startsWith variable" in {
    val customer = Customer(Some("sebastian"), None, None, None)
    assertThat({ aCustomer:Customer => aCustomer.name}).startsWith("sebas").otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    assertThat({ aCustomer:Customer => aCustomer.name}).startsWith("SEBAS").otherwise("sebastian does not start with SEBAS")
      .evaluate(customer)
      .expectsToBeFalseWith("sebastian does not start with SEBAS")
    assertThat({ aCustomer:Customer => aCustomer.name}).startsWith("sebas ").otherwise("sebastian does not start with sebas ")
      .evaluate(customer)
      .expectsToBeFalseWith("sebastian does not start with sebas ")
    assertThat({ aCustomer:Customer => aCustomer.name}).startsWith(" sebas").otherwise("sebastian does not start with  sebas")
      .evaluate(customer)
      .expectsToBeFalseWith("sebastian does not start with  sebas")
  }

  it should "startsWithIgnoringCase constant" in {
    assertThat(None:Option[String]).startsWithIgnoringCase("SEBAS").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("sebastian")).startsWithIgnoringCase("SEBAS").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("SEBASTIAN")).startsWithIgnoringCase("sebas").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("sebastian")).startsWithIgnoringCase("sebas ").otherwise("sebastian does not start with sebas ")
      .expectsToBeFalseWith("sebastian does not start with sebas ")
    assertThat(Some("sebastian")).startsWithIgnoringCase(" sebas").otherwise("sebastian does not start with  sebas")
      .expectsToBeFalseWith("sebastian does not start with  sebas")
  }

  it should "startsWithIgnoringCase variable" in {
    val customer = Customer(Some("sebastian"), None, None, None)
    assertThat({ aCustomer:Customer => aCustomer.name}).startsWithIgnoringCase("SEBAS").otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    assertThat({ aCustomer:Customer => aCustomer.name}).startsWithIgnoringCase("sebas ").otherwise("sebastian does not start with sebas ")
      .evaluate(customer)
      .expectsToBeFalseWith("sebastian does not start with sebas ")
    assertThat({ aCustomer:Customer => aCustomer.name}).startsWithIgnoringCase(" sebas").otherwise("sebastian does not start with  sebas")
      .evaluate(customer)
      .expectsToBeFalseWith("sebastian does not start with  sebas")
    val anotherCustomer = Customer(Some("SEBASTIAN"), None, None, None)
    assertThat({ aCustomer:Customer => aCustomer.name}).startsWithIgnoringCase("sebas").otherwise("No way it fails...")
      .evaluate(anotherCustomer)
      .expectsToBeTrue()
  }

  it should "endsWith constant" in {
    assertThat(None:Option[String]).endsWith("tian").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("Sebastian")).endsWith("tian").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("Sebastian")).endsWith("TIAN").otherwise("Sebastian does not end with TIAN")
      .expectsToBeFalseWith("Sebastian does not end with TIAN")
    assertThat(Some("Sebastian")).endsWith(" tian").otherwise("Sebastian does not end with  tian")
      .expectsToBeFalseWith("Sebastian does not end with  tian")
    assertThat(Some("Sebastian")).endsWith("tian ").otherwise("Sebastian does not end with tian ")
      .expectsToBeFalseWith("Sebastian does not end with tian ")
  }

  it should "endsWith variable" in {
    val customer = Customer(Some("sebastian"), None, None, None)
    assertThat({ aCustomer:Customer => aCustomer.name}).endsWith("tian").otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    assertThat({ aCustomer:Customer => aCustomer.name}).endsWith("TIAN").otherwise("sebastian does not end with TIAN")
      .evaluate(customer)
      .expectsToBeFalseWith("sebastian does not end with TIAN")
    assertThat({ aCustomer:Customer => aCustomer.name}).endsWith("tian ").otherwise("sebastian does not end with tian ")
      .evaluate(customer)
      .expectsToBeFalseWith("sebastian does not end with tian ")
    assertThat({ aCustomer:Customer => aCustomer.name}).endsWith(" tian").otherwise("sebastian does not end with  tian")
      .evaluate(customer)
      .expectsToBeFalseWith("sebastian does not end with  tian")
  }

  it should "endsWithIgnoringCase constant" in {
    assertThat(None:Option[String]).endsWithIgnoringCase("TIAN").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("sebastian")).endsWithIgnoringCase("TIAN").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("SEBASTIAN")).endsWithIgnoringCase("tian").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("sebastian")).endsWithIgnoringCase("tian ").otherwise("sebastian does not end with tian ")
      .expectsToBeFalseWith("sebastian does not end with tian ")
    assertThat(Some("sebastian")).endsWithIgnoringCase(" tian").otherwise("sebastian does not end with  tian")
      .expectsToBeFalseWith("sebastian does not end with  tian")
  }

  it should "endsWithIgnoringCase variable" in {
    val customer = Customer(Some("sebastian"), None, None, None)
    assertThat({ aCustomer:Customer => aCustomer.name}).endsWithIgnoringCase("TIAN").otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    assertThat({ aCustomer:Customer => aCustomer.name}).endsWithIgnoringCase("tian ").otherwise("sebastian does not end with tian ")
      .evaluate(customer)
      .expectsToBeFalseWith("sebastian does not end with tian ")
    assertThat({ aCustomer:Customer => aCustomer.name}).endsWithIgnoringCase(" tian").otherwise("sebastian does not end with  tian")
      .evaluate(customer)
      .expectsToBeFalseWith("sebastian does not end with  tian")
    val anotherCustomer = Customer(Some("SEBASTIAN"), None, None, None)
    assertThat({ aCustomer:Customer => aCustomer.name}).endsWithIgnoringCase("tian").otherwise("No way it fails...")
      .evaluate(anotherCustomer)
      .expectsToBeTrue()
  }

  it should "contains constant" in {
    assertThat(None:Option[String]).contains("basti").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("Sebastian")).contains("basti").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("Sebastian")).contains("BASTI").otherwise("Sebastian does not contain BASTI")
      .expectsToBeFalseWith("Sebastian does not contain BASTI")
    assertThat(Some("Sebastian")).contains(" basti").otherwise("Sebastian does not contain  basti")
      .expectsToBeFalseWith("Sebastian does not contain  basti")
    assertThat(Some("Sebastian")).contains("basti ").otherwise("Sebastian does not contain basti ")
      .expectsToBeFalseWith("Sebastian does not contain basti ")
  }

  it should "contains variable" in {
    val customer = Customer(Some("Sebastian"), None, None, None)
    assertThat({ aCustomer:Customer => aCustomer.name}).contains("basti").otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    assertThat({ aCustomer:Customer => aCustomer.name}).contains("BASTI").otherwise("Sebastian does not contain BASTI")
      .evaluate(customer)
      .expectsToBeFalseWith("Sebastian does not contain BASTI")
    assertThat({ aCustomer:Customer => aCustomer.name}).contains(" basti").otherwise("Sebastian does not contain  basti")
      .evaluate(customer)
      .expectsToBeFalseWith("Sebastian does not contain  basti")
    assertThat({ aCustomer:Customer => aCustomer.name}).contains("basti ").otherwise("Sebastian does not contain basti ")
      .evaluate(customer)
      .expectsToBeFalseWith("Sebastian does not contain basti ")
  }

  it should "containsIgnoringCase constant" in {
    assertThat(None:Option[String]).containsIgnoringCase("basti").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("SEBASTIAN")).containsIgnoringCase("basti").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("Sebastian")).containsIgnoringCase("BASTI").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("Sebastian")).containsIgnoringCase(" basti").otherwise("Sebastian does not contain  basti")
      .expectsToBeFalseWith("Sebastian does not contain  basti")
    assertThat(Some("Sebastian")).containsIgnoringCase("basti ").otherwise("Sebastian does not contain basti ")
      .expectsToBeFalseWith("Sebastian does not contain basti ")
  }

  it should "containsIgnoringCase variable" in {
    val customer = Customer(Some("Sebastian"), None, None, None)
    assertThat({ aCustomer:Customer => aCustomer.name}).containsIgnoringCase("basti").otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    assertThat({ aCustomer:Customer => aCustomer.name}).containsIgnoringCase(" basti").otherwise("Sebastian does not contain  basti")
      .evaluate(customer)
      .expectsToBeFalseWith("Sebastian does not contain  basti")
    assertThat({ aCustomer:Customer => aCustomer.name}).containsIgnoringCase("basti ").otherwise("Sebastian does not contain basti ")
      .evaluate(customer)
      .expectsToBeFalseWith("Sebastian does not contain basti ")
    val anotherCustomer = Customer(Some("SEBASTIAN"), None, None, None)
    assertThat({ aCustomer:Customer => aCustomer.name}).containsIgnoringCase("basti").otherwise("No way it fails...")
      .evaluate(anotherCustomer)
      .expectsToBeTrue()
  }

  it should "isEmail constant" in {
    assertThat(None:Option[String]).isEmail.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("user@domain.com")).isEmail.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("user@domain.co.in")).isEmail.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("user.name@domain.com")).isEmail.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("user_name@domain.com")).isEmail.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("username@yahoo.corporate.in")).isEmail.otherwise("No way it fails...")
      .expectsToBeTrue()

    assertThat(Some("")).isEmail.otherwise("malformed email")
      .expectsToBeFalseWith("malformed email")
    assertThat(Some("@yahoo.com")).isEmail.otherwise("malformed email")
      .expectsToBeFalseWith("malformed email")
    assertThat(Some("username@")).isEmail.otherwise("malformed email")
      .expectsToBeFalseWith("malformed email")
    assertThat(Some(".username@yahoo.com")).isEmail.otherwise("malformed email")
      .expectsToBeFalseWith("malformed email")
    assertThat(Some("username@yahoo.com.")).isEmail.otherwise("malformed email")
      .expectsToBeFalseWith("malformed email")
    assertThat(Some("username@yahoo..com")).isEmail.otherwise("malformed email")
      .expectsToBeFalseWith("malformed email")
    assertThat(Some("username@yahoo.c")).isEmail.otherwise("malformed email")
      .expectsToBeFalseWith("malformed email")
    assertThat(Some("username@yahoo.corporate")).isEmail.otherwise("malformed email")
      .expectsToBeFalseWith("malformed email")
  }

  it should "isEmail variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), None, None)
    assertThat({ customer:Customer => customer.email}).isEmail.otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()

    val anotherCustomer = Customer(Some("david"), Some("david@email.c"), None, None)
    assertThat({ customer:Customer => customer.email}).isEmail.otherwise("malformed email")
      .evaluate(anotherCustomer)
      .expectsToBeFalseWith("malformed email")
  }

  it should "isUri constant" in {
    assertThat(None:Option[String]).isUri.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("http://google.com")).isUri.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("http://google.com.ar")).isUri.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("www.google.com")).isUri.otherwise("protocol missing")
      .expectsToBeFalseWith("protocol missing")
    assertThat(Some("https://google.com")).isUri.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("http://www.google.com")).isUri.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("http://goo gle.com")).isUri.otherwise("malformed uri")
      .expectsToBeFalseWith("malformed uri")
    assertThat(Some("http://goo$gle.com.ar")).isUri.otherwise("malformed uri")
      .expectsToBeFalseWith("malformed uri")
  }

  it should "isUri variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("http://www.mystory.com"), None)
    assertThat({ customer:Customer => customer.homepage}).isUri.otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer(Some("david"), Some("david@email.com"), Some("www.mys$tory.com"), None)
    assertThat({ customer:Customer => customer.homepage}).isUri.otherwise("malformed uri")
      .evaluate(anotherCustomer)
      .expectsToBeFalseWith("malformed uri")
  }

  it should "isAlphanumeric constant" in {
    assertThat(None:Option[String]).isAlphanumeric.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("abc123")).isAlphanumeric.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("abc")).isAlphanumeric.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("123")).isAlphanumeric.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("abc$123")).isAlphanumeric.otherwise("not alphanumeric")
      .expectsToBeFalseWith("not alphanumeric")
    assertThat(Some("abc 123")).isAlphanumeric.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("abc.123")).isAlphanumeric.otherwise("not alphanumeric")
      .expectsToBeFalseWith("not alphanumeric")
    assertThat(Some("")).isAlphanumeric.otherwise("not alphanumeric")
      .expectsToBeFalseWith("not alphanumeric")
  }

  it should "isAlphanumeric variable" in {
    val customer = Customer(Some("sebastian123"), Some("sebastian@email.com"), Some("www.google.com"), None)
    assertThat({ customer:Customer => customer.name}).isAlphanumeric.otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer(Some("seb$astian"), Some("sebastian@email.com"), Some("www.google.com"), None)
    assertThat({ customer:Customer => customer.name}).isAlphanumeric.otherwise("not alphanumeric")
      .evaluate(anotherCustomer)
      .expectsToBeFalseWith("not alphanumeric")
  }

  it should "isAlphabetic constant" in {
    assertThat(None:Option[String]).isAlphabetic.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("abc")).isAlphabetic.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("ABC")).isAlphabetic.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("xyz")).isAlphabetic.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("XYZ")).isAlphabetic.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("")).isAlphabetic.otherwise("not alphabetic")
      .expectsToBeFalseWith("not alphabetic")
    assertThat(Some("123")).isAlphabetic.otherwise("not alphabetic")
      .expectsToBeFalseWith("not alphabetic")
    assertThat(Some("$")).isAlphabetic.otherwise("not alphabetic")
      .expectsToBeFalseWith("not alphabetic")
    assertThat(Some("ab%c")).isAlphabetic.otherwise("not alphabetic")
      .expectsToBeFalseWith("not alphabetic")
    assertThat(Some("ab1c")).isAlphabetic.otherwise("not alphabetic")
      .expectsToBeFalseWith("not alphabetic")
  }

  it should "isAlphabetic variant" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@gmail.com"), Some("www.sebastian.com"), None)
    assertThat({ customer:Customer => customer.name}).isAlphabetic.otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer(Some("dav3e"), Some("dave@gmail.com"), Some("www.dave.com"), None)
    assertThat({ customer:Customer => customer.name}).isAlphabetic.otherwise("not alphabetic")
      .evaluate(anotherCustomer)
      .expectsToBeFalseWith("not alphabetic")
  }

  it should "isNumber constant" in {
    assertThat(None:Option[String]).isNumber.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("123")).isNumber.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("")).isNumber.otherwise("not a number")
      .expectsToBeFalseWith("not a number")
    assertThat(Some("123 ")).isNumber.otherwise("not a number")
      .expectsToBeFalseWith("not a number")
    assertThat(Some("12$3")).isNumber.otherwise("not a number")
      .expectsToBeFalseWith("not a number")
    assertThat(Some("12a3")).isNumber.otherwise("not a number")
      .expectsToBeFalseWith("not a number")
  }

  it should "isNumber variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
    assertThat({ customer:Customer => customer.age}).isNumber.otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer(Some("dave"), Some("dave@email.com"), Some("www.dave.com"), Some("3a7"))
    assertThat({ customer:Customer => customer.age}).isNumber.otherwise("not a number")
      .evaluate(anotherCustomer)
      .expectsToBeFalseWith("not a number")
  }

  it should "isSameLengthAs constant" in {
    assertThat(None:Option[String]).isSameLengthAs(9).otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("sebastian")).isSameLengthAs(9).otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("sebastian")).isSameLengthAs(8).otherwise("boom!")
      .expectsToBeFalseWith("boom!")
    assertThat(Some("sebastian")).isSameLengthAs(10).otherwise("boom!")
      .expectsToBeFalseWith("boom!")
  }

  it should "isSameLengthAs variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
    assertThat({ customer:Customer => customer.name}).isSameLengthAs(9).otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    assertThat({ customer:Customer => customer.name}).isSameLengthAs(8).otherwise("boom!")
      .evaluate(customer)
      .expectsToBeFalseWith("boom!")
    assertThat({ customer:Customer => customer.name}).isSameLengthAs(10).otherwise("boom!")
      .evaluate(customer)
      .expectsToBeFalseWith("boom!")
  }

  it should "isLongerThan constant" in {
    assertThat(None:Option[String]).isLongerThan(8).otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("sebastian")).isLongerThan(8).otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("sebastian")).isLongerThan(9).otherwise("boom!")
      .expectsToBeFalseWith("boom!")
    assertThat(Some("sebastian")).isLongerThan(10).otherwise("boom!")
      .expectsToBeFalseWith("boom!")
  }

  it should "isLongerThan variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
    assertThat({ customer:Customer => customer.name}).isLongerThan(8).otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    assertThat({ customer:Customer => customer.name}).isLongerThan(9).otherwise("boom!")
      .evaluate(customer)
      .expectsToBeFalseWith("boom!")
    assertThat({ customer:Customer => customer.name}).isLongerThan(10).otherwise("boom!")
      .evaluate(customer)
      .expectsToBeFalseWith("boom!")
  }

  it should "isLongerThanOrEqualTo constant" in {
    assertThat(None:Option[String]).isLongerThanOrEqualTo(8).otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("sebastian")).isLongerThanOrEqualTo(8).otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("sebastian")).isLongerThanOrEqualTo(9).otherwise("boom!")
      .expectsToBeTrue()
    assertThat(Some("sebastian")).isLongerThanOrEqualTo(10).otherwise("boom!")
      .expectsToBeFalseWith("boom!")
  }

  it should "isLongerThanOrEqualTo variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
    assertThat({ customer:Customer => customer.name}).isLongerThanOrEqualTo(8).otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    assertThat({ customer:Customer => customer.name}).isLongerThanOrEqualTo(9).otherwise("boom!")
      .evaluate(customer)
      .expectsToBeTrue()
    assertThat({ customer:Customer => customer.name}).isLongerThanOrEqualTo(10).otherwise("boom!")
      .evaluate(customer)
      .expectsToBeFalseWith("boom!")
  }

  it should "isShorterThan constant" in {
    assertThat(None:Option[String]).isShorterThan(10).otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("sebastian")).isShorterThan(10).otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("sebastian")).isShorterThan(9).otherwise("boom!")
      .expectsToBeFalseWith("boom!")
    assertThat(Some("sebastian")).isShorterThan(8).otherwise("boom!")
      .expectsToBeFalseWith("boom!")
  }

  it should "isShorterThan variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
    assertThat({ customer:Customer => customer.name}).isShorterThan(10).otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    assertThat({ customer:Customer => customer.name}).isShorterThan(9).otherwise("boom!")
      .evaluate(customer)
      .expectsToBeFalseWith("boom!")
    assertThat({ customer:Customer => customer.name}).isShorterThan(8).otherwise("boom!")
      .evaluate(customer)
      .expectsToBeFalseWith("boom!")
  }

  it should "isShorterThanOrEqualTo constant" in {
    assertThat(None:Option[String]).isShorterThanOrEqualTo(10).otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("sebastian")).isShorterThanOrEqualTo(10).otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("sebastian")).isShorterThanOrEqualTo(9).otherwise("boom!")
      .expectsToBeTrue()
    assertThat(Some("sebastian")).isShorterThanOrEqualTo(8).otherwise("boom!")
      .expectsToBeFalseWith("boom!")
  }

  it should "isShorterThanOrEqualTo variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
    assertThat({ customer:Customer => customer.name}).isShorterThanOrEqualTo(10).otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    assertThat({ customer:Customer => customer.name}).isShorterThanOrEqualTo(9).otherwise("boom!")
      .evaluate(customer)
      .expectsToBeTrue()
    assertThat({ customer:Customer => customer.name}).isShorterThanOrEqualTo(8).otherwise("boom!")
      .evaluate(customer)
      .expectsToBeFalseWith("boom!")
  }

  it should "isBlank constant" in {
    assertThat(None:Option[String]).isBlank.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("")).isBlank.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("  ")).isBlank.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(Some("a")).isBlank.otherwise("boom!")
      .expectsToBeFalseWith("boom!")
  }

  it should "isBlank variable" in {
    val customer = Customer(Some(" "), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
    assertThat({ customer:Customer => customer.name}).isBlank.otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    assertThat({ customer:Customer => customer.name}).isBlank.otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
  }

  it should "isNotBlank constant" in {
    assertThat(None:Option[String]).isNotBlank.otherwise("boom!")
      .expectsToBeTrue()
    assertThat(Some("a")).isNotBlank.otherwise("boom!")
      .expectsToBeTrue()
    assertThat(Some("")).isNotBlank.otherwise("boom!")
      .expectsToBeFalseWith("boom!")
    assertThat(Some("  ")).isNotBlank.otherwise("boom!")
      .expectsToBeFalseWith("boom!")
  }

  it should "isNotBlank variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
    assertThat({ customer:Customer => customer.name}).isNotBlank.otherwise("boom!")
      .evaluate(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer(Some("  "), Some("dave@email.com"), Some("www.dave.com"), Some("35"))
    assertThat({ customer:Customer => customer.name}).isNotBlank.otherwise("boom!")
      .evaluate(anotherCustomer)
      .expectsToBeFalseWith("boom!")
  }
}
