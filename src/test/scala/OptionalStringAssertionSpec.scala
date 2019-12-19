import assertion.Assert
import org.scalatest.{FlatSpec, Matchers}
import assertion.Assertion._

class OptionalStringAssertionSpec extends FlatSpec with Matchers {

  case class Customer(name: Option[String], email: Option[String], homepage: Option[String], age: Option[String])

  it should "is defined" in {
    Assert.assert(that(Some("sebastian")).isDefined.otherwise("No way it fails...")).expectsToBeTrue()
    Assert.assert(that(Some("sebastian")).isDefined.startsWith("seba").otherwise("No way it fails...")).expectsToBeTrue()
    Assert.assert(that(Some("sebastian")).isDefined.startsWith("eba").otherwise("Boom!")).expectsToBeFalseWith("Boom!")
    Assert.assert(that(None).isDefined.otherwise("Boom!")).expectsToBeFalseWith("Boom!")
  }

  it should "equalTo constant" in {
    Assert
      .assert(that(None).isEqualTo("Sebastian").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("Sebastian")).isEqualTo("Sebastian").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("Sebastian")).isEqualTo("sebastian").otherwise("Sebastian is not equal to sebastian"))
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
    Assert
      .assert(that(Some("Sebastian")).isEqualTo(" sebastian").otherwise("Sebastian is not equal to sebastian"))
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
    Assert
      .assert(that(Some("Sebastian")).isEqualTo("sebastian ").otherwise("Sebastian is not equal to sebastian"))
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
    Assert
      .assert(that(Some(" Sebastian")).isEqualTo("sebastian").otherwise("Sebastian is not equal to sebastian"))
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
    Assert
      .assert(that(Some("Sebastian ")).isEqualTo("sebastian").otherwise("Sebastian is not equal to sebastian"))
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
  }

  it should "equalTo variable" in {
    val customer = Customer(Some("Sebastian"), None, None, None)
    Assert
      .assert(that({ aCustomer:Customer => aCustomer.name}).isEqualTo("Sebastian").otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({ aCustomer:Customer => aCustomer.name}).isEqualTo("sebastian").otherwise("Sebastian is not equal to sebastian"))
      .in(customer)
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
  }

  it should "equalToIgnoringCase constant" in {
    Assert
      .assert(that(None).isEqualToIgnoringCase("Sebastian").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("Sebastian")).isEqualToIgnoringCase("Sebastian").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).isEqualToIgnoringCase("SEBASTIAN").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("SEBASTIAN")).isEqualToIgnoringCase("sebastian").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some(" Sebastian")).isEqualToIgnoringCase("sebastian").otherwise(" Sebastian is not equal to sebastian"))
      .expectsToBeFalseWith(" Sebastian is not equal to sebastian")
    Assert
      .assert(that(Some("Sebastian ")).isEqualToIgnoringCase("sebastian").otherwise("Sebastian  is not equal to sebastian"))
      .expectsToBeFalseWith("Sebastian  is not equal to sebastian")
    Assert
      .assert(that(Some("Sebastian")).isEqualToIgnoringCase(" Sebastian").otherwise("Sebastian is not equal to  sebastian"))
      .expectsToBeFalseWith("Sebastian is not equal to  sebastian")
    Assert
      .assert(that(Some("Sebastian")).isEqualToIgnoringCase("Sebastian ").otherwise("Sebastian is not equal to sebastian "))
      .expectsToBeFalseWith("Sebastian is not equal to sebastian ")
  }

  it should "equalToIgnoringCase variable" in {
    val customer = Customer(Some("sebastian"), None, None, None)
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
      .assert(that(None).startsWith("Sebas").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("Sebastian")).startsWith("Sebas").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("Sebastian")).startsWith("sebas").otherwise("Sebastian does not start with sebas"))
      .expectsToBeFalseWith("Sebastian does not start with sebas")
    Assert
      .assert(that(Some("Sebastian")).startsWith(" Sebas").otherwise("Sebastian does not start with  Sebas"))
      .expectsToBeFalseWith("Sebastian does not start with  Sebas")
    Assert
      .assert(that(Some("Sebastian")).startsWith("Sebas ").otherwise("Sebastian does not start with Sebas "))
      .expectsToBeFalseWith("Sebastian does not start with Sebas ")
  }

  it should "startsWith variable" in {
    val customer = Customer(Some("sebastian"), None, None, None)
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
      .assert(that(None).startsWithIgnoringCase("SEBAS").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).startsWithIgnoringCase("SEBAS").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("SEBASTIAN")).startsWithIgnoringCase("sebas").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).startsWithIgnoringCase("sebas ").otherwise("sebastian does not start with sebas "))
      .expectsToBeFalseWith("sebastian does not start with sebas ")
    Assert
      .assert(that(Some("sebastian")).startsWithIgnoringCase(" sebas").otherwise("sebastian does not start with  sebas"))
      .expectsToBeFalseWith("sebastian does not start with  sebas")
  }

  it should "startsWithIgnoringCase variable" in {
    val customer = Customer(Some("sebastian"), None, None, None)
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
    val anotherCustomer = Customer(Some("SEBASTIAN"), None, None, None)
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).startsWithIgnoringCase("sebas").otherwise("No way it fails..."))
      .in(anotherCustomer)
      .expectsToBeTrue()
  }

  it should "endsWith constant" in {
    Assert
      .assert(that(None).endsWith("tian").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("Sebastian")).endsWith("tian").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("Sebastian")).endsWith("TIAN").otherwise("Sebastian does not end with TIAN"))
      .expectsToBeFalseWith("Sebastian does not end with TIAN")
    Assert
      .assert(that(Some("Sebastian")).endsWith(" tian").otherwise("Sebastian does not end with  tian"))
      .expectsToBeFalseWith("Sebastian does not end with  tian")
    Assert
      .assert(that(Some("Sebastian")).endsWith("tian ").otherwise("Sebastian does not end with tian "))
      .expectsToBeFalseWith("Sebastian does not end with tian ")
  }

  it should "endsWith variable" in {
    val customer = Customer(Some("sebastian"), None, None, None)
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
      .assert(that(None).endsWithIgnoringCase("TIAN").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).endsWithIgnoringCase("TIAN").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("SEBASTIAN")).endsWithIgnoringCase("tian").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).endsWithIgnoringCase("tian ").otherwise("sebastian does not end with tian "))
      .expectsToBeFalseWith("sebastian does not end with tian ")
    Assert
      .assert(that(Some("sebastian")).endsWithIgnoringCase(" tian").otherwise("sebastian does not end with  tian"))
      .expectsToBeFalseWith("sebastian does not end with  tian")
  }

  it should "endsWithIgnoringCase variable" in {
    val customer = Customer(Some("sebastian"), None, None, None)
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
    val anotherCustomer = Customer(Some("SEBASTIAN"), None, None, None)
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).endsWithIgnoringCase("tian").otherwise("No way it fails..."))
      .in(anotherCustomer)
      .expectsToBeTrue()
  }

  it should "contains constant" in {
    Assert
      .assert(that(None).contains("basti").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("Sebastian")).contains("basti").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("Sebastian")).contains("BASTI").otherwise("Sebastian does not contain BASTI"))
      .expectsToBeFalseWith("Sebastian does not contain BASTI")
    Assert
      .assert(that(Some("Sebastian")).contains(" basti").otherwise("Sebastian does not contain  basti"))
      .expectsToBeFalseWith("Sebastian does not contain  basti")
    Assert
      .assert(that(Some("Sebastian")).contains("basti ").otherwise("Sebastian does not contain basti "))
      .expectsToBeFalseWith("Sebastian does not contain basti ")
  }

  it should "contains variable" in {
    val customer = Customer(Some("Sebastian"), None, None, None)
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
      .assert(that(None).containsIgnoringCase("basti").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("SEBASTIAN")).containsIgnoringCase("basti").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("Sebastian")).containsIgnoringCase("BASTI").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("Sebastian")).containsIgnoringCase(" basti").otherwise("Sebastian does not contain  basti"))
      .expectsToBeFalseWith("Sebastian does not contain  basti")
    Assert
      .assert(that(Some("Sebastian")).containsIgnoringCase("basti ").otherwise("Sebastian does not contain basti "))
      .expectsToBeFalseWith("Sebastian does not contain basti ")
  }

  it should "containsIgnoringCase variable" in {
    val customer = Customer(Some("Sebastian"), None, None, None)
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
    val anotherCustomer = Customer(Some("SEBASTIAN"), None, None, None)
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).containsIgnoringCase("basti").otherwise("No way it fails..."))
      .in(anotherCustomer)
      .expectsToBeTrue()
  }

  it should "isEmail constant" in {
    Assert
      .assert(that(None).isEmail.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("user@domain.com")).isEmail.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("user@domain.co.in")).isEmail.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("user.name@domain.com")).isEmail.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("user_name@domain.com")).isEmail.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("username@yahoo.corporate.in")).isEmail.otherwise("No way it fails..."))
      .expectsToBeTrue()

    Assert
      .assert(that(Some("")).isEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
    Assert
      .assert(that(Some("@yahoo.com")).isEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
    Assert
      .assert(that(Some("username@")).isEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
    Assert
      .assert(that(Some(".username@yahoo.com")).isEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
    Assert
      .assert(that(Some("username@yahoo.com.")).isEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
    Assert
      .assert(that(Some("username@yahoo..com")).isEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
    Assert
      .assert(that(Some("username@yahoo.c")).isEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
    Assert
      .assert(that(Some("username@yahoo.corporate")).isEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
  }

  it should "isEmail variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), None, None)
    Assert
      .assert(that({customer:Customer => customer.email}).isEmail.otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()

    val anotherCustomer = Customer(Some("david"), Some("david@email.c"), None, None)
    Assert
      .assert(that({customer:Customer => customer.email}).isEmail.otherwise("malformed email"))
      .in(anotherCustomer)
      .expectsToBeFalseWith("malformed email")
  }

  it should "isUri constant" in {
    Assert
      .assert(that(None).isUri.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("http://google.com")).isUri.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("http://google.com.ar")).isUri.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("www.google.com")).isUri.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("https://google.com")).isUri.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("http://www.google.com")).isUri.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("http://goo gle.com")).isUri.otherwise("malformed uri"))
      .expectsToBeFalseWith("malformed uri")
    Assert
      .assert(that(Some("http://google.com.ar.")).isUri.otherwise("malformed uri"))
      .expectsToBeFalseWith("malformed uri")
    Assert
      .assert(that(Some("http://goo$gle.com.ar")).isUri.otherwise("malformed uri"))
      .expectsToBeFalseWith("malformed uri")
  }

  it should "isUri variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.mystory.com"), None)
    Assert
      .assert(that({customer:Customer => customer.homepage}).isUri.otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer(Some("david"), Some("david@email.com"), Some("www.mys$tory.com"), None)
    Assert
      .assert(that({customer:Customer => customer.homepage}).isUri.otherwise("malformed uri"))
      .in(anotherCustomer)
      .expectsToBeFalseWith("malformed uri")
  }

  it should "isAlphanumeric constant" in {
    Assert
      .assert(that(None).isAlphanumeric.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("abc123")).isAlphanumeric.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("abc")).isAlphanumeric.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("123")).isAlphanumeric.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("abc$123")).isAlphanumeric.otherwise("not alphanumeric"))
      .expectsToBeFalseWith("not alphanumeric")
    Assert
      .assert(that(Some("abc 123")).isAlphanumeric.otherwise("not alphanumeric"))
      .expectsToBeFalseWith("not alphanumeric")
    Assert
      .assert(that(Some("abc.123")).isAlphanumeric.otherwise("not alphanumeric"))
      .expectsToBeFalseWith("not alphanumeric")
    Assert
      .assert(that(Some("")).isAlphanumeric.otherwise("not alphanumeric"))
      .expectsToBeFalseWith("not alphanumeric")
  }

  it should "isAlphanumeric variable" in {
    val customer = Customer(Some("sebastian123"), Some("sebastian@email.com"), Some("www.google.com"), None)
    Assert
      .assert(that({customer:Customer => customer.name}).isAlphanumeric.otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer(Some("seb$astian"), Some("sebastian@email.com"), Some("www.google.com"), None)
    Assert
      .assert(that({customer:Customer => customer.name}).isAlphanumeric.otherwise("not alphanumeric"))
      .in(anotherCustomer)
      .expectsToBeFalseWith("not alphanumeric")
  }

  it should "isAlphabetic constant" in {
    Assert
      .assert(that(None).isAlphabetic.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("abc")).isAlphabetic.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("ABC")).isAlphabetic.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("xyz")).isAlphabetic.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("XYZ")).isAlphabetic.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("")).isAlphabetic.otherwise("not alphabetic"))
      .expectsToBeFalseWith("not alphabetic")
    Assert
      .assert(that(Some("123")).isAlphabetic.otherwise("not alphabetic"))
      .expectsToBeFalseWith("not alphabetic")
    Assert
      .assert(that(Some("$")).isAlphabetic.otherwise("not alphabetic"))
      .expectsToBeFalseWith("not alphabetic")
    Assert
      .assert(that(Some("ab%c")).isAlphabetic.otherwise("not alphabetic"))
      .expectsToBeFalseWith("not alphabetic")
    Assert
      .assert(that(Some("ab1c")).isAlphabetic.otherwise("not alphabetic"))
      .expectsToBeFalseWith("not alphabetic")
  }

  it should "isAlphabetic variant" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@gmail.com"), Some("www.sebastian.com"), None)
    Assert
      .assert(that({customer:Customer => customer.name}).isAlphabetic.otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer(Some("dav3e"), Some("dave@gmail.com"), Some("www.dave.com"), None)
    Assert
      .assert(that({customer:Customer => customer.name}).isAlphabetic.otherwise("not alphabetic"))
      .in(anotherCustomer)
      .expectsToBeFalseWith("not alphabetic")
  }

  it should "isNumber constant" in {
    Assert
      .assert(that(None).isNumber.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("123")).isNumber.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("")).isNumber.otherwise("not a number"))
      .expectsToBeFalseWith("not a number")
    Assert
      .assert(that(Some("123 ")).isNumber.otherwise("not a number"))
      .expectsToBeFalseWith("not a number")
    Assert
      .assert(that(Some("12$3")).isNumber.otherwise("not a number"))
      .expectsToBeFalseWith("not a number")
    Assert
      .assert(that(Some("12a3")).isNumber.otherwise("not a number"))
      .expectsToBeFalseWith("not a number")
  }

  it should "isNumber variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
    Assert
      .assert(that({customer:Customer => customer.age}).isNumber.otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer(Some("dave"), Some("dave@email.com"), Some("www.dave.com"), Some("3a7"))
    Assert
      .assert(that({customer:Customer => customer.age}).isNumber.otherwise("not a number"))
      .in(anotherCustomer)
      .expectsToBeFalseWith("not a number")
  }

  it should "isSameLengthAs constant" in {
    Assert
      .assert(that(None).isSameLengthAs(9).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).isSameLengthAs(9).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).isSameLengthAs(8).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
    Assert
      .assert(that(Some("sebastian")).isSameLengthAs(10).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
  }

  it should "isSameLengthAs variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
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
      .assert(that(None).isLongerThan(8).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).isLongerThan(8).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).isLongerThan(9).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
    Assert
      .assert(that(Some("sebastian")).isLongerThan(10).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
  }

  it should "isLongerThan variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
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
      .assert(that(None).isLongerThanOrEqualTo(8).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).isLongerThanOrEqualTo(8).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).isLongerThanOrEqualTo(9).otherwise("boom!"))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).isLongerThanOrEqualTo(10).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
  }

  it should "isLongerThanOrEqualTo variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
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
      .assert(that(None).isShorterThan(10).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).isShorterThan(10).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).isShorterThan(9).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
    Assert
      .assert(that(Some("sebastian")).isShorterThan(8).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
  }

  it should "isShorterThan variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
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
      .assert(that(None).isShorterThanOrEqualTo(10).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).isShorterThanOrEqualTo(10).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).isShorterThanOrEqualTo(9).otherwise("boom!"))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).isShorterThanOrEqualTo(8).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
  }

  it should "isShorterThanOrEqualTo variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
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
      .assert(that(None).isBlank.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("")).isBlank.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("  ")).isBlank.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("a")).isBlank.otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
  }

  it should "isBlank variable" in {
    val customer = Customer(Some(" "), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
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
      .assert(that(None).isNotBlank.otherwise("boom!"))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("a")).isNotBlank.otherwise("boom!"))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("")).isNotBlank.otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
    Assert
      .assert(that(Some("  ")).isNotBlank.otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
  }

  it should "isNotBlank variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
    Assert
      .assert(that({customer:Customer => customer.name}).isNotBlank.otherwise("boom!"))
      .in(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer(Some("  "), Some("dave@email.com"), Some("www.dave.com"), Some("35"))
    Assert
      .assert(that({customer:Customer => customer.name}).isNotBlank.otherwise("boom!"))
      .in(anotherCustomer)
      .expectsToBeFalseWith("boom!")
  }
}
