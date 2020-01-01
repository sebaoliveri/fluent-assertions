
import assertion.Assert
import assertion.AssertionBuilder._
import org.scalatest.{FlatSpec, Matchers}
import expression.{QuantifiableExp, StringExp}
import extension.QuantifiableExt._

class StringAssertionSpec extends FlatSpec with Matchers {

  case class Customer(name: String, email: String, homepage: String, age: String)

  case class User(name: String, email: String, password: String) {
    Assert.assert(that(name).isNotBlank.otherwise("The name is required")
      .and(that(name).isAlphabetic.otherwise("The name must contain alphabetic chars only"))
      .and(that(email).isNotBlank.isEmail.otherwise("The email is required"))
      .and(that(email).isEmail.otherwise("The email is not valid"))
      .and(that(password).isNotBlank.otherwise("The password is expected"))
      .and(that(password).isLongerThanOrEqualTo(7).isShorterThanOrEqualTo(20).otherwise("Password length must be between 7 and 20")))
      .signalIfFailed()
  }

  it should "pasar cosas" in {
    val customerAges: Iterable[Int] = List(5, 35, 28)

    Assert.assert(that(customerAges).forAll(_.isInInclusiveRange(4, 100)).otherwise("caca")).signalIfFailed()
  }

  it should "isEqualTo constant" in {
    Assert
      .assert(that("Sebastian").isEqualTo("Sebastian").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("Sebastian").isEqualTo("sebastian").otherwise("Sebastian is not equal to sebastian"))
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
    Assert
      .assert(that("Sebastian").isEqualTo(" sebastian").otherwise("Sebastian is not equal to sebastian"))
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
    Assert
      .assert(that("Sebastian").isEqualTo("sebastian ").otherwise("Sebastian is not equal to sebastian"))
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
    Assert
      .assert(that(" Sebastian").isEqualTo("sebastian").otherwise("Sebastian is not equal to sebastian"))
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
    Assert
      .assert(that("Sebastian ").isEqualTo("sebastian").otherwise("Sebastian is not equal to sebastian"))
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
  }

  it should "isEqualTo variable" in {
    val customer = Customer("Sebastian", "sebastian@email.com", "www.mystory.com", "37")
    Assert
      .assert(that({ aCustomer:Customer => aCustomer.name}).isEqualTo("Sebastian").otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({ aCustomer:Customer => aCustomer.name}).isEqualTo("sebastian").otherwise("Sebastian is not equal to sebastian"))
      .in(customer)
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
  }

  it should "isEqualToIgnoringCase constant" in {
    Assert
      .assert(that("Sebastian").isEqualToIgnoringCase("Sebastian").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("sebastian").isEqualToIgnoringCase("SEBASTIAN").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("SEBASTIAN").isEqualToIgnoringCase("sebastian").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(" Sebastian").isEqualToIgnoringCase("sebastian").otherwise(" Sebastian is not equal to sebastian"))
      .expectsToBeFalseWith(" Sebastian is not equal to sebastian")
    Assert
      .assert(that("Sebastian ").isEqualToIgnoringCase("sebastian").otherwise("Sebastian  is not equal to sebastian"))
      .expectsToBeFalseWith("Sebastian  is not equal to sebastian")
    Assert
      .assert(that("Sebastian").isEqualToIgnoringCase(" Sebastian").otherwise("Sebastian is not equal to  sebastian"))
      .expectsToBeFalseWith("Sebastian is not equal to  sebastian")
    Assert
      .assert(that("Sebastian").isEqualToIgnoringCase("Sebastian ").otherwise("Sebastian is not equal to sebastian "))
      .expectsToBeFalseWith("Sebastian is not equal to sebastian ")
  }

  it should "isEqualToIgnoringCase variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.mystory.com", "37")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).isEqualToIgnoringCase("SEBASTIAN").otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).isEqualToIgnoringCase("SEBASTIAN ").otherwise("Sebastian is not equal to sebastian "))
      .in(customer)
      .expectsToBeFalseWith("Sebastian is not equal to sebastian ")
  }

  it should "startsWith constant" in {

    Assert
      .assert(that("Sebastian").startsWith("Sebas").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("Sebastian").startsWith("sebas").otherwise("Sebastian does not start with sebas"))
      .expectsToBeFalseWith("Sebastian does not start with sebas")
    Assert
      .assert(that("Sebastian").startsWith(" Sebas").otherwise("Sebastian does not start with  Sebas"))
      .expectsToBeFalseWith("Sebastian does not start with  Sebas")
    Assert
      .assert(that("Sebastian").startsWith("Sebas ").otherwise("Sebastian does not start with Sebas "))
      .expectsToBeFalseWith("Sebastian does not start with Sebas ")
  }

  it should "startsWith variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.mystory.com", "37")

    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).startsWith("sebas").otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).startsWith("SEBAS").otherwise("sebastian does not start with SEBAS"))
      .in(customer)
      .expectsToBeFalseWith("sebastian does not start with SEBAS")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).startsWith("sebas ").otherwise("sebastian does not start with sebas "))
      .in(customer)
      .expectsToBeFalseWith("sebastian does not start with sebas ")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).startsWith(" sebas").otherwise("sebastian does not start with  sebas"))
      .in(customer)
      .expectsToBeFalseWith("sebastian does not start with  sebas")
  }

  it should "startsWithIgnoringCase constant" in {
    Assert
      .assert(that("sebastian").startsWithIgnoringCase("SEBAS").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("SEBASTIAN").startsWithIgnoringCase("sebas").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("sebastian").startsWithIgnoringCase("sebas ").otherwise("sebastian does not start with sebas "))
      .expectsToBeFalseWith("sebastian does not start with sebas ")
    Assert
      .assert(that("sebastian").startsWithIgnoringCase(" sebas").otherwise("sebastian does not start with  sebas"))
      .expectsToBeFalseWith("sebastian does not start with  sebas")
  }

  it should "startsWithIgnoringCase variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.mystory.com", "37")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).startsWithIgnoringCase("SEBAS").otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).startsWithIgnoringCase("sebas ").otherwise("sebastian does not start with sebas "))
      .in(customer)
      .expectsToBeFalseWith("sebastian does not start with sebas ")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).startsWithIgnoringCase(" sebas").otherwise("sebastian does not start with  sebas"))
      .in(customer)
      .expectsToBeFalseWith("sebastian does not start with  sebas")
    val anotherCustomer = Customer("SEBASTIAN", "sebastian@email.com", "www.mystory.com", "37")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).startsWithIgnoringCase("sebas").otherwise("No way it fails..."))
      .in(anotherCustomer)
      .expectsToBeTrue()
  }

  it should "endsWith constant" in {
    Assert
      .assert(that("Sebastian").endsWith("tian").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("Sebastian").endsWith("TIAN").otherwise("Sebastian does not end with TIAN"))
      .expectsToBeFalseWith("Sebastian does not end with TIAN")
    Assert
      .assert(that("Sebastian").endsWith(" tian").otherwise("Sebastian does not end with  tian"))
      .expectsToBeFalseWith("Sebastian does not end with  tian")
    Assert
      .assert(that("Sebastian").endsWith("tian ").otherwise("Sebastian does not end with tian "))
      .expectsToBeFalseWith("Sebastian does not end with tian ")
  }

  it should "endsWith variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.mystory.com", "37")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).endsWith("tian").otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).endsWith("TIAN").otherwise("sebastian does not end with TIAN"))
      .in(customer)
      .expectsToBeFalseWith("sebastian does not end with TIAN")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).endsWith("tian ").otherwise("sebastian does not end with tian "))
      .in(customer)
      .expectsToBeFalseWith("sebastian does not end with tian ")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).endsWith(" tian").otherwise("sebastian does not end with  tian"))
      .in(customer)
      .expectsToBeFalseWith("sebastian does not end with  tian")
  }

  it should "endsWithIgnoringCase constant" in {
    Assert
      .assert(that("sebastian").endsWithIgnoringCase("TIAN").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("SEBASTIAN").endsWithIgnoringCase("tian").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("sebastian").endsWithIgnoringCase("tian ").otherwise("sebastian does not end with tian "))
      .expectsToBeFalseWith("sebastian does not end with tian ")
    Assert
      .assert(that("sebastian").endsWithIgnoringCase(" tian").otherwise("sebastian does not end with  tian"))
      .expectsToBeFalseWith("sebastian does not end with  tian")
  }

  it should "endsWithIgnoringCase variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.mystory.com", "37")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).endsWithIgnoringCase("TIAN").otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).endsWithIgnoringCase("tian ").otherwise("sebastian does not end with tian "))
      .in(customer)
      .expectsToBeFalseWith("sebastian does not end with tian ")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).endsWithIgnoringCase(" tian").otherwise("sebastian does not end with  tian"))
      .in(customer)
      .expectsToBeFalseWith("sebastian does not end with  tian")
    val anotherCustomer = Customer("SEBASTIAN", "sebastian@email.com", "www.mystory.com", "37")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).endsWithIgnoringCase("tian").otherwise("No way it fails..."))
      .in(anotherCustomer)
      .expectsToBeTrue()
  }

  it should "contains constant" in {
    Assert
      .assert(that("Sebastian").contains("basti").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("Sebastian").contains("BASTI").otherwise("Sebastian does not contain BASTI"))
      .expectsToBeFalseWith("Sebastian does not contain BASTI")
    Assert
      .assert(that("Sebastian").contains(" basti").otherwise("Sebastian does not contain  basti"))
      .expectsToBeFalseWith("Sebastian does not contain  basti")
    Assert
      .assert(that("Sebastian").contains("basti ").otherwise("Sebastian does not contain basti "))
      .expectsToBeFalseWith("Sebastian does not contain basti ")
  }

  it should "contains variable" in {
    val customer = Customer("Sebastian", "sebastian@email.com", "www.mystory.com", "37")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).contains("basti").otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).contains("BASTI").otherwise("Sebastian does not contain BASTI"))
      .in(customer)
      .expectsToBeFalseWith("Sebastian does not contain BASTI")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).contains(" basti").otherwise("Sebastian does not contain  basti"))
      .in(customer)
      .expectsToBeFalseWith("Sebastian does not contain  basti")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).contains("basti ").otherwise("Sebastian does not contain basti "))
      .in(customer)
      .expectsToBeFalseWith("Sebastian does not contain basti ")
  }

  it should "containsIgnoringCase constant" in {
    Assert
      .assert(that("SEBASTIAN").containsIgnoringCase("basti").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("Sebastian").containsIgnoringCase("BASTI").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("Sebastian").containsIgnoringCase(" basti").otherwise("Sebastian does not contain  basti"))
      .expectsToBeFalseWith("Sebastian does not contain  basti")
    Assert
      .assert(that("Sebastian").containsIgnoringCase("basti ").otherwise("Sebastian does not contain basti "))
      .expectsToBeFalseWith("Sebastian does not contain basti ")
  }

  it should "containsIgnoringCase variable" in {
    val customer = Customer("Sebastian", "sebastian@email.com", "www.mystory.com", "37")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).containsIgnoringCase("basti").otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).containsIgnoringCase(" basti").otherwise("Sebastian does not contain  basti"))
      .in(customer)
      .expectsToBeFalseWith("Sebastian does not contain  basti")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).containsIgnoringCase("basti ").otherwise("Sebastian does not contain basti "))
      .in(customer)
      .expectsToBeFalseWith("Sebastian does not contain basti ")
    val anotherCustomer = Customer("SEBASTIAN", "sebastian@email.com", "www.mystory.com", "37")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).containsIgnoringCase("basti").otherwise("No way it fails..."))
      .in(anotherCustomer)
      .expectsToBeTrue()
  }

  it should "isEmail constant" in {
    Assert
      .assert(that("user@domain.com").isEmail.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("user@domain.co.in").isEmail.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("user.name@domain.com").isEmail.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("user_name@domain.com").isEmail.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("username@yahoo.corporate.in").isEmail.otherwise("No way it fails..."))
      .expectsToBeTrue()

    Assert
      .assert(that("").isEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
    Assert
      .assert(that("@yahoo.com").isEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
    Assert
      .assert(that("username@").isEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
    Assert
      .assert(that(".username@yahoo.com").isEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
    Assert
      .assert(that("username@yahoo.com.").isEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
    Assert
      .assert(that("username@yahoo..com").isEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
    Assert
      .assert(that("username@yahoo.c").isEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
    Assert
      .assert(that("username@yahoo.corporate").isEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
  }

  it should "isEmail variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.mystory.com", "37")
    Assert
      .assert(that({customer:Customer => customer.email}).isEmail.otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()

    val anotherCustomer = Customer("david", "david@email.c", "www.mystory.com", "37")
    Assert
      .assert(that({customer:Customer => customer.email}).isEmail.otherwise("malformed email"))
      .in(anotherCustomer)
      .expectsToBeFalseWith("malformed email")
  }

  it should "isUrl constant" in {
    Assert
      .assert(that("http://google.com").isUri.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("http://google.com.ar").isUri.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("www.google.com").isUri.otherwise("Protocol missing"))
      .expectsToBeFalseWith("Protocol missing")
    Assert
      .assert(that("https://google.com").isUri.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("http://www.google.com").isUri.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("http://goo gle.com").isUri.otherwise("malformed uri"))
      .expectsToBeFalseWith("malformed uri")
    Assert
      .assert(that("http://goo$gle.com.ar").isUri.otherwise("malformed uri"))
      .expectsToBeFalseWith("malformed uri")
  }

  it should "isUrl variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "http://www.mystory.com", "37")
    Assert
      .assert(that({customer:Customer => customer.homepage}).isUri.otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer("david", "david@email.com", "www.mys$tory.com", "37")
    Assert
      .assert(that({customer:Customer => customer.homepage}).isUri.otherwise("malformed uri"))
      .in(anotherCustomer)
      .expectsToBeFalseWith("malformed uri")
  }

  it should "isAlphanumeric constant" in {
    Assert
      .assert(that("abc123").isAlphanumeric.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("abc").isAlphanumeric.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("123").isAlphanumeric.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("abc$123").isAlphanumeric.otherwise("not alphanumeric"))
      .expectsToBeFalseWith("not alphanumeric")
    Assert
      .assert(that("abc.123").isAlphanumeric.otherwise("not alphanumeric"))
      .expectsToBeFalseWith("not alphanumeric")
    Assert
      .assert(that("").isAlphanumeric.otherwise("not alphanumeric"))
      .expectsToBeFalseWith("not alphanumeric")
  }

  it should "isAlphanumeric variable" in {
    val customer = Customer("sebastian123", "sebastian@email.com", "www.google.com", "37")
    Assert
      .assert(that({customer:Customer => customer.name}).isAlphanumeric.otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer("seb$astian", "sebastian@email.com", "www.google.com", "37")
    Assert
      .assert(that({customer:Customer => customer.name}).isAlphanumeric.otherwise("not alphanumeric"))
      .in(anotherCustomer)
      .expectsToBeFalseWith("not alphanumeric")
  }

  it should "isAlphabetic constant" in {
    Assert
      .assert(that("abc").isAlphabetic.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("ABC").isAlphabetic.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("xyz").isAlphabetic.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("XYZ").isAlphabetic.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("").isAlphabetic.otherwise("not alphabetic"))
      .expectsToBeFalseWith("not alphabetic")
    Assert
      .assert(that("123").isAlphabetic.otherwise("not alphabetic"))
      .expectsToBeFalseWith("not alphabetic")
    Assert
      .assert(that("$").isAlphabetic.otherwise("not alphabetic"))
      .expectsToBeFalseWith("not alphabetic")
    Assert
      .assert(that("ab%c").isAlphabetic.otherwise("not alphabetic"))
      .expectsToBeFalseWith("not alphabetic")
    Assert
      .assert(that("ab1c").isAlphabetic.otherwise("not alphabetic"))
      .expectsToBeFalseWith("not alphabetic")
  }

  it should "isAlphabetic variant" in {
    val customer = Customer("sebastian", "sebastian@gmail.com", "www.sebastian.com", "37")
    Assert
      .assert(that({customer:Customer => customer.name}).isAlphabetic.otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer("dav3e", "dave@gmail.com", "www.dave.com", "37")
    Assert
      .assert(that({customer:Customer => customer.name}).isAlphabetic.otherwise("not alphabetic"))
      .in(anotherCustomer)
      .expectsToBeFalseWith("not alphabetic")
  }

  it should "isNumber constant" in {
    Assert
      .assert(that("123").isNumber.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("").isNumber.otherwise("not a number"))
      .expectsToBeFalseWith("not a number")
    Assert
      .assert(that("123 ").isNumber.otherwise("not a number"))
      .expectsToBeFalseWith("not a number")
    Assert
      .assert(that("12$3").isNumber.otherwise("not a number"))
      .expectsToBeFalseWith("not a number")
    Assert
      .assert(that("12a3").isNumber.otherwise("not a number"))
      .expectsToBeFalseWith("not a number")
  }

  it should "isNumber variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.sebastian.com", "37")
    Assert
      .assert(that({customer:Customer => customer.age}).isNumber.otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer("dave", "dave@email.com", "www.dave.com", "3a7")
    Assert
      .assert(that({customer:Customer => customer.age}).isNumber.otherwise("not a number"))
      .in(anotherCustomer)
      .expectsToBeFalseWith("not a number")
  }

  it should "isSameLengthAs constant" in {
    Assert
      .assert(that("sebastian").isSameLengthAs(9).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("sebastian").isSameLengthAs(8).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
    Assert
      .assert(that("sebastian").isSameLengthAs(10).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
  }

  it should "isSameLengthAs variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.sebastian.com", "37")
    Assert
      .assert(that({customer:Customer => customer.name}).isSameLengthAs(9).otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({customer:Customer => customer.name}).isSameLengthAs(8).otherwise("boom!"))
      .in(customer)
      .expectsToBeFalseWith("boom!")
    Assert
      .assert(that({customer:Customer => customer.name}).isSameLengthAs(10).otherwise("boom!"))
      .in(customer)
      .expectsToBeFalseWith("boom!")
  }

  it should "isLongerThan constant" in {
    Assert
      .assert(that("sebastian").isLongerThan(8).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("sebastian").isLongerThan(9).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
    Assert
      .assert(that("sebastian").isLongerThan(10).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
  }

  it should "isLongerThan variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.sebastian.com", "37")
    Assert
      .assert(that({customer:Customer => customer.name}).isLongerThan(8).otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({customer:Customer => customer.name}).isLongerThan(9).otherwise("boom!"))
      .in(customer)
      .expectsToBeFalseWith("boom!")
    Assert
      .assert(that({customer:Customer => customer.name}).isLongerThan(10).otherwise("boom!"))
      .in(customer)
      .expectsToBeFalseWith("boom!")
  }

  it should "isLongerThanOrEqualTo constant" in {
    Assert
      .assert(that("sebastian").isLongerThanOrEqualTo(8).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("sebastian").isLongerThanOrEqualTo(9).otherwise("boom!"))
      .expectsToBeTrue()
    Assert
      .assert(that("sebastian").isLongerThanOrEqualTo(10).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
  }

  it should "isLongerThanOrEqualTo variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.sebastian.com", "37")
    Assert
      .assert(that({customer:Customer => customer.name}).isLongerThanOrEqualTo(8).otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({customer:Customer => customer.name}).isLongerThanOrEqualTo(9).otherwise("boom!"))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({customer:Customer => customer.name}).isLongerThanOrEqualTo(10).otherwise("boom!"))
      .in(customer)
      .expectsToBeFalseWith("boom!")
  }

  it should "isShorterThan constant" in {
    Assert
      .assert(that("sebastian").isShorterThan(10).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("sebastian").isShorterThan(9).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
    Assert
      .assert(that("sebastian").isShorterThan(8).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
  }

  it should "isShorterThan variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.sebastian.com", "37")
    Assert
      .assert(that({customer:Customer => customer.name}).isShorterThan(10).otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({customer:Customer => customer.name}).isShorterThan(9).otherwise("boom!"))
      .in(customer)
      .expectsToBeFalseWith("boom!")
    Assert
      .assert(that({customer:Customer => customer.name}).isShorterThan(8).otherwise("boom!"))
      .in(customer)
      .expectsToBeFalseWith("boom!")
  }

  it should "isShorterThanOrEqualTo constant" in {
    Assert
      .assert(that("sebastian").isShorterThanOrEqualTo(10).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("sebastian").isShorterThanOrEqualTo(9).otherwise("boom!"))
      .expectsToBeTrue()
    Assert
      .assert(that("sebastian").isShorterThanOrEqualTo(8).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
  }

  it should "isShorterThanOrEqualTo variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.sebastian.com", "37")
    Assert
      .assert(that({customer:Customer => customer.name}).isShorterThanOrEqualTo(10).otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({customer:Customer => customer.name}).isShorterThanOrEqualTo(9).otherwise("boom!"))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({customer:Customer => customer.name}).isShorterThanOrEqualTo(8).otherwise("boom!"))
      .in(customer)
      .expectsToBeFalseWith("boom!")
  }

  it should "isBlank constant" in {
    Assert
      .assert(that("").isBlank.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("  ").isBlank.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that("a").isBlank.otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
  }

  it should "isBlank variable" in {
    val customer = Customer(" ", "sebastian@email.com", "www.sebastian.com", "37")
    Assert
      .assert(that({customer:Customer => customer.name}).isBlank.otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({customer:Customer => customer.name}).isBlank.otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
  }

  it should "isNotBlank constant" in {
    Assert
      .assert(that("a").isNotBlank.otherwise("boom!"))
      .expectsToBeTrue()
    Assert
      .assert(that("").isNotBlank.otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
    Assert
      .assert(that("  ").isNotBlank.otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
  }

  it should "isNotBlank variable" in {
    val customer = Customer("sebastian", "sebastian@email.com", "www.sebastian.com", "37")
    Assert
      .assert(that({customer:Customer => customer.name}).isNotBlank.otherwise("boom!"))
      .in(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer("  ", "dave@email.com", "www.dave.com", "35")
    Assert
      .assert(that({customer:Customer => customer.name}).isNotBlank.otherwise("boom!"))
      .in(anotherCustomer)
      .expectsToBeFalseWith("boom!")
  }
}
