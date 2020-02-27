
import assertion.AssertionBuilder._
import org.scalatest.{FlatSpec, Matchers}

class StringAssertionSpec extends FlatSpec with Matchers {

  case class Customer(name: String, email: String, homepage: String, age: String)

  it should "isEqualTo constant" in {
    assertThat("Sebastian").isEqualTo("Sebastian").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("Sebastian").isEqualTo("sebastian").otherwise("Sebastian is not equal to sebastian")
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
    assertThat("Sebastian").isEqualTo(" sebastian").otherwise("Sebastian is not equal to sebastian")
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
    assertThat("Sebastian").isEqualTo("sebastian ").otherwise("Sebastian is not equal to sebastian")
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
    assertThat(" Sebastian").isEqualTo("sebastian").otherwise("Sebastian is not equal to sebastian")
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
    assertThat("Sebastian ").isEqualTo("sebastian").otherwise("Sebastian is not equal to sebastian")
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
  }

  it should "isEqualTo variable" in {
    val customer = Customer("Sebastian", "sebastian@email.com", "www.mystory.com", "37")
    assertThat({ aCustomer:Customer => aCustomer.name}).isEqualTo("Sebastian").otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    assertThat({ aCustomer:Customer => aCustomer.name}).isEqualTo("sebastian").otherwise("Sebastian is not equal to sebastian")
      .evaluate(customer)
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
  }

  it should "isEqualToIgnoringCase constant" in {
    assertThat("Sebastian").isEqualToIgnoringCase("Sebastian").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("sebastian").isEqualToIgnoringCase("SEBASTIAN").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("SEBASTIAN").isEqualToIgnoringCase("sebastian").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat(" Sebastian").isEqualToIgnoringCase("sebastian").otherwise(" Sebastian is not equal to sebastian")
      .expectsToBeFalseWith(" Sebastian is not equal to sebastian")
    assertThat("Sebastian ").isEqualToIgnoringCase("sebastian").otherwise("Sebastian  is not equal to sebastian")
      .expectsToBeFalseWith("Sebastian  is not equal to sebastian")
    assertThat("Sebastian").isEqualToIgnoringCase(" Sebastian").otherwise("Sebastian is not equal to  sebastian")
      .expectsToBeFalseWith("Sebastian is not equal to  sebastian")
    assertThat("Sebastian").isEqualToIgnoringCase("Sebastian ").otherwise("Sebastian is not equal to sebastian ")
      .expectsToBeFalseWith("Sebastian is not equal to sebastian ")
  }

  it should "isEqualToIgnoringCase variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.mystory.com", "37")
    assertThat({ aCustomer:Customer => aCustomer.name}).isEqualToIgnoringCase("SEBASTIAN").otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    assertThat({ aCustomer:Customer => aCustomer.name}).isEqualToIgnoringCase("SEBASTIAN ").otherwise("Sebastian is not equal to sebastian ")
      .evaluate(customer)
      .expectsToBeFalseWith("Sebastian is not equal to sebastian ")
  }

  it should "startsWith constant" in {
    assertThat("Sebastian").startsWith("Sebas").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("Sebastian").startsWith("sebas").otherwise("Sebastian does not start with sebas")
      .expectsToBeFalseWith("Sebastian does not start with sebas")
    assertThat("Sebastian").startsWith(" Sebas").otherwise("Sebastian does not start with  Sebas")
      .expectsToBeFalseWith("Sebastian does not start with  Sebas")
    assertThat("Sebastian").startsWith("Sebas ").otherwise("Sebastian does not start with Sebas ")
      .expectsToBeFalseWith("Sebastian does not start with Sebas ")
  }

  it should "startsWith variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.mystory.com", "37")

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
    assertThat("sebastian").startsWithIgnoringCase("SEBAS").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("SEBASTIAN").startsWithIgnoringCase("sebas").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("sebastian").startsWithIgnoringCase("sebas ").otherwise("sebastian does not start with sebas ")
      .expectsToBeFalseWith("sebastian does not start with sebas ")
    assertThat("sebastian").startsWithIgnoringCase(" sebas").otherwise("sebastian does not start with  sebas")
      .expectsToBeFalseWith("sebastian does not start with  sebas")
  }

  it should "startsWithIgnoringCase variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.mystory.com", "37")
    assertThat({ aCustomer:Customer => aCustomer.name}).startsWithIgnoringCase("SEBAS").otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    assertThat({ aCustomer:Customer => aCustomer.name}).startsWithIgnoringCase("sebas ").otherwise("sebastian does not start with sebas ")
      .evaluate(customer)
      .expectsToBeFalseWith("sebastian does not start with sebas ")
    assertThat({ aCustomer:Customer => aCustomer.name}).startsWithIgnoringCase(" sebas").otherwise("sebastian does not start with  sebas")
      .evaluate(customer)
      .expectsToBeFalseWith("sebastian does not start with  sebas")
    val anotherCustomer = Customer("SEBASTIAN", "sebastian@email.com", "www.mystory.com", "37")
    assertThat({ aCustomer:Customer => aCustomer.name}).startsWithIgnoringCase("sebas").otherwise("No way it fails...")
      .evaluate(anotherCustomer)
      .expectsToBeTrue()
  }

  it should "endsWith constant" in {
    assertThat("Sebastian").endsWith("tian").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("Sebastian").endsWith("TIAN").otherwise("Sebastian does not end with TIAN")
      .expectsToBeFalseWith("Sebastian does not end with TIAN")
    assertThat("Sebastian").endsWith(" tian").otherwise("Sebastian does not end with  tian")
      .expectsToBeFalseWith("Sebastian does not end with  tian")
    assertThat("Sebastian").endsWith("tian ").otherwise("Sebastian does not end with tian ")
      .expectsToBeFalseWith("Sebastian does not end with tian ")
  }

  it should "endsWith variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.mystory.com", "37")
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
    assertThat("sebastian").endsWithIgnoringCase("TIAN").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("SEBASTIAN").endsWithIgnoringCase("tian").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("sebastian").endsWithIgnoringCase("tian ").otherwise("sebastian does not end with tian ")
      .expectsToBeFalseWith("sebastian does not end with tian ")
    assertThat("sebastian").endsWithIgnoringCase(" tian").otherwise("sebastian does not end with  tian")
      .expectsToBeFalseWith("sebastian does not end with  tian")
  }

  it should "endsWithIgnoringCase variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.mystory.com", "37")
    assertThat({ aCustomer:Customer => aCustomer.name}).endsWithIgnoringCase("TIAN").otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    assertThat({ aCustomer:Customer => aCustomer.name}).endsWithIgnoringCase("tian ").otherwise("sebastian does not end with tian ")
      .evaluate(customer)
      .expectsToBeFalseWith("sebastian does not end with tian ")
    assertThat({ aCustomer:Customer => aCustomer.name}).endsWithIgnoringCase(" tian").otherwise("sebastian does not end with  tian")
      .evaluate(customer)
      .expectsToBeFalseWith("sebastian does not end with  tian")
    val anotherCustomer = Customer("SEBASTIAN", "sebastian@email.com", "www.mystory.com", "37")
    assertThat({ aCustomer:Customer => aCustomer.name}).endsWithIgnoringCase("tian").otherwise("No way it fails...")
      .evaluate(anotherCustomer)
      .expectsToBeTrue()
  }

  it should "contains constant" in {
    assertThat("Sebastian").contains("basti").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("Sebastian").contains("BASTI").otherwise("Sebastian does not contain BASTI")
      .expectsToBeFalseWith("Sebastian does not contain BASTI")
    assertThat("Sebastian").contains(" basti").otherwise("Sebastian does not contain  basti")
      .expectsToBeFalseWith("Sebastian does not contain  basti")
    assertThat("Sebastian").contains("basti ").otherwise("Sebastian does not contain basti ")
      .expectsToBeFalseWith("Sebastian does not contain basti ")
  }

  it should "contains variable" in {
    val customer = Customer("Sebastian", "sebastian@email.com", "www.mystory.com", "37")
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
    assertThat("SEBASTIAN").containsIgnoringCase("basti").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("Sebastian").containsIgnoringCase("BASTI").otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("Sebastian").containsIgnoringCase(" basti").otherwise("Sebastian does not contain  basti")
      .expectsToBeFalseWith("Sebastian does not contain  basti")
    assertThat("Sebastian").containsIgnoringCase("basti ").otherwise("Sebastian does not contain basti ")
      .expectsToBeFalseWith("Sebastian does not contain basti ")
  }

  it should "containsIgnoringCase variable" in {
    val customer = Customer("Sebastian", "sebastian@email.com", "www.mystory.com", "37")
    assertThat({ aCustomer:Customer => aCustomer.name}).containsIgnoringCase("basti").otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    assertThat({ aCustomer:Customer => aCustomer.name}).containsIgnoringCase(" basti").otherwise("Sebastian does not contain  basti")
      .evaluate(customer)
      .expectsToBeFalseWith("Sebastian does not contain  basti")
    assertThat({ aCustomer:Customer => aCustomer.name}).containsIgnoringCase("basti ").otherwise("Sebastian does not contain basti ")
      .evaluate(customer)
      .expectsToBeFalseWith("Sebastian does not contain basti ")
    val anotherCustomer = Customer("SEBASTIAN", "sebastian@email.com", "www.mystory.com", "37")
    assertThat({ aCustomer:Customer => aCustomer.name}).containsIgnoringCase("basti").otherwise("No way it fails...")
      .evaluate(anotherCustomer)
      .expectsToBeTrue()
  }

  it should "isEmail constant" in {
    assertThat("user@domain.com").isEmail.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("user@domain.co.in").isEmail.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("user.name@domain.com").isEmail.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("user_name@domain.com").isEmail.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("username@yahoo.corporate.in").isEmail.otherwise("No way it fails...")
      .expectsToBeTrue()

    assertThat("").isEmail.otherwise("malformed email")
      .expectsToBeFalseWith("malformed email")
    assertThat("@yahoo.com").isEmail.otherwise("malformed email")
      .expectsToBeFalseWith("malformed email")
    assertThat("username@").isEmail.otherwise("malformed email")
      .expectsToBeFalseWith("malformed email")
    assertThat(".username@yahoo.com").isEmail.otherwise("malformed email")
      .expectsToBeFalseWith("malformed email")
    assertThat("username@yahoo.com.").isEmail.otherwise("malformed email")
      .expectsToBeFalseWith("malformed email")
    assertThat("username@yahoo..com").isEmail.otherwise("malformed email")
      .expectsToBeFalseWith("malformed email")
    assertThat("username@yahoo.c").isEmail.otherwise("malformed email")
      .expectsToBeFalseWith("malformed email")
    assertThat("username@yahoo.corporate").isEmail.otherwise("malformed email")
      .expectsToBeFalseWith("malformed email")
  }

  it should "isEmail variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.mystory.com", "37")
    assertThat({ customer:Customer => customer.email}).isEmail.otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()

    val anotherCustomer = Customer("david", "david@email.c", "www.mystory.com", "37")
    assertThat({ customer:Customer => customer.email}).isEmail.otherwise("malformed email")
      .evaluate(anotherCustomer)
      .expectsToBeFalseWith("malformed email")
  }

  it should "isUrl constant" in {
    assertThat("http://google.com").isUri.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("http://google.com.ar").isUri.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("www.google.com").isUri.otherwise("Protocol missing")
      .expectsToBeFalseWith("Protocol missing")
    assertThat("https://google.com").isUri.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("http://www.google.com").isUri.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("http://goo gle.com").isUri.otherwise("malformed uri")
      .expectsToBeFalseWith("malformed uri")
    assertThat("http://goo$gle.com.ar").isUri.otherwise("malformed uri")
      .expectsToBeFalseWith("malformed uri")
  }

  it should "isUrl variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "http://www.mystory.com", "37")
    assertThat({ customer:Customer => customer.homepage}).isUri.otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer("david", "david@email.com", "www.mys$tory.com", "37")
    assertThat({ customer:Customer => customer.homepage}).isUri.otherwise("malformed uri")
      .evaluate(anotherCustomer)
      .expectsToBeFalseWith("malformed uri")
  }

  it should "isAlphanumeric constant" in {
    assertThat("abc123").isAlphanumeric.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("abc").isAlphanumeric.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("123").isAlphanumeric.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("abc$123").isAlphanumeric.otherwise("not alphanumeric")
      .expectsToBeFalseWith("not alphanumeric")
    assertThat("abc.123").isAlphanumeric.otherwise("not alphanumeric")
      .expectsToBeFalseWith("not alphanumeric")
    assertThat("").isAlphanumeric.otherwise("not alphanumeric")
      .expectsToBeFalseWith("not alphanumeric")
  }

  it should "isAlphanumeric variable" in {
    val customer = Customer("sebastian123", "sebastian@email.com", "www.google.com", "37")
    assertThat({ customer:Customer => customer.name}).isAlphanumeric.otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer("seb$astian", "sebastian@email.com", "www.google.com", "37")
    assertThat({ customer:Customer => customer.name}).isAlphanumeric.otherwise("not alphanumeric")
      .evaluate(anotherCustomer)
      .expectsToBeFalseWith("not alphanumeric")
  }

  it should "isAlphabetic constant" in {
    assertThat("abc").isAlphabetic.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("ABC").isAlphabetic.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("xyz").isAlphabetic.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("XYZ").isAlphabetic.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("").isAlphabetic.otherwise("not alphabetic")
      .expectsToBeFalseWith("not alphabetic")
    assertThat("123").isAlphabetic.otherwise("not alphabetic")
      .expectsToBeFalseWith("not alphabetic")
    assertThat("$").isAlphabetic.otherwise("not alphabetic")
      .expectsToBeFalseWith("not alphabetic")
    assertThat("ab%c").isAlphabetic.otherwise("not alphabetic")
      .expectsToBeFalseWith("not alphabetic")
    assertThat("ab1c").isAlphabetic.otherwise("not alphabetic")
      .expectsToBeFalseWith("not alphabetic")
  }

  it should "isAlphabetic variant" in {
    val customer = Customer("sebastian", "sebastian@gmail.com", "www.sebastian.com", "37")
    assertThat({ customer:Customer => customer.name}).isAlphabetic.otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer("dav3e", "dave@gmail.com", "www.dave.com", "37")
    assertThat({ customer:Customer => customer.name}).isAlphabetic.otherwise("not alphabetic")
      .evaluate(anotherCustomer)
      .expectsToBeFalseWith("not alphabetic")
  }

  it should "isNumber constant" in {
    assertThat("123").isNumber.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("").isNumber.otherwise("not a number")
      .expectsToBeFalseWith("not a number")
    assertThat("123 ").isNumber.otherwise("not a number")
      .expectsToBeFalseWith("not a number")
    assertThat("12$3").isNumber.otherwise("not a number")
      .expectsToBeFalseWith("not a number")
    assertThat("12a3").isNumber.otherwise("not a number")
      .expectsToBeFalseWith("not a number")
  }

  it should "isNumber variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.sebastian.com", "37")
    assertThat({ customer:Customer => customer.age}).isNumber.otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer("dave", "dave@email.com", "www.dave.com", "3a7")
    assertThat({ customer:Customer => customer.age}).isNumber.otherwise("not a number")
      .evaluate(anotherCustomer)
      .expectsToBeFalseWith("not a number")
  }

  it should "isSameLengthAs constant" in {
    assertThat("sebastian").isSameLengthAs(9).otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("sebastian").isSameLengthAs(8).otherwise("boom!")
      .expectsToBeFalseWith("boom!")
    assertThat("sebastian").isSameLengthAs(10).otherwise("boom!")
      .expectsToBeFalseWith("boom!")
  }

  it should "isSameLengthAs variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.sebastian.com", "37")
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
    assertThat("sebastian").isLongerThan(8).otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("sebastian").isLongerThan(9).otherwise("boom!")
      .expectsToBeFalseWith("boom!")
    assertThat("sebastian").isLongerThan(10).otherwise("boom!")
      .expectsToBeFalseWith("boom!")
  }

  it should "isLongerThan variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.sebastian.com", "37")
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
    assertThat("sebastian").isLongerThanOrEqualTo(8).otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("sebastian").isLongerThanOrEqualTo(9).otherwise("boom!")
      .expectsToBeTrue()
    assertThat("sebastian").isLongerThanOrEqualTo(10).otherwise("boom!")
      .expectsToBeFalseWith("boom!")
  }

  it should "isLongerThanOrEqualTo variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.sebastian.com", "37")
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
    assertThat("sebastian").isShorterThan(10).otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("sebastian").isShorterThan(9).otherwise("boom!")
      .expectsToBeFalseWith("boom!")
    assertThat("sebastian").isShorterThan(8).otherwise("boom!")
      .expectsToBeFalseWith("boom!")
  }

  it should "isShorterThan variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.sebastian.com", "37")
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
    assertThat("sebastian").isShorterThanOrEqualTo(10).otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("sebastian").isShorterThanOrEqualTo(9).otherwise("boom!")
      .expectsToBeTrue()
    assertThat("sebastian").isShorterThanOrEqualTo(8).otherwise("boom!")
      .expectsToBeFalseWith("boom!")
  }

  it should "isShorterThanOrEqualTo variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.sebastian.com", "37")
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
    assertThat("").isBlank.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("  ").isBlank.otherwise("No way it fails...")
      .expectsToBeTrue()
    assertThat("a").isBlank.otherwise("boom!")
      .expectsToBeFalseWith("boom!")
  }

  it should "isBlank variable" in {
    val customer = Customer(" ", "sebastian@email.com", "www.sebastian.com", "37")
    assertThat({ customer:Customer => customer.name}).isBlank.otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
    assertThat({ customer:Customer => customer.name}).isBlank.otherwise("No way it fails...")
      .evaluate(customer)
      .expectsToBeTrue()
  }

  it should "isNotBlank constant" in {
    assertThat("a").isNotBlank.otherwise("boom!")
      .expectsToBeTrue()
    assertThat("").isNotBlank.otherwise("boom!")
      .expectsToBeFalseWith("boom!")
    assertThat("  ").isNotBlank.otherwise("boom!")
      .expectsToBeFalseWith("boom!")
  }

  it should "isNotBlank variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.sebastian.com", "37")
    assertThat({ customer:Customer => customer.name}).isNotBlank.otherwise("boom!")
      .evaluate(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer("  ", "dave@email.com", "www.dave.com", "35")
    assertThat({ customer:Customer => customer.name}).isNotBlank.otherwise("boom!")
      .evaluate(anotherCustomer)
      .expectsToBeFalseWith("boom!")
  }
}
