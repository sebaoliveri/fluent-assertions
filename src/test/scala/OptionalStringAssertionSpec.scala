import assertion.Assert
import org.scalatest.{FlatSpec, Matchers}
import assertion.Assertion._

class OptionalStringAssertionSpec extends FlatSpec with Matchers {

  case class Customer(name: Option[String], email: Option[String], homepage: Option[String], age: Option[String])

  it should "is defined" in {
    Assert.assert(that(Some("sebastian")).isDefined.otherwise("No way it fails...")).expectsToBeTrue()
    Assert.assert(that(Some("sebastian")).isDefined.and.startsWith("seba").otherwise("No way it fails...")).expectsToBeTrue()
    Assert.assert(that(Some("sebastian")).isDefined.and.startsWith("eba").otherwise("Boom!")).expectsToBeFalseWith("Boom!")
    Assert.assert(that(None).isDefined.otherwise("Boom!")).expectsToBeFalseWith("Boom!")
  }

  it should "wouldBeEqualTo constant" in {
    Assert
      .assert(that(None).wouldBeEqualTo("Sebastian").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("Sebastian")).wouldBeEqualTo("Sebastian").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("Sebastian")).wouldBeEqualTo("sebastian").otherwise("Sebastian is not equal to sebastian"))
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
    Assert
      .assert(that(Some("Sebastian")).wouldBeEqualTo(" sebastian").otherwise("Sebastian is not equal to sebastian"))
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
    Assert
      .assert(that(Some("Sebastian")).wouldBeEqualTo("sebastian ").otherwise("Sebastian is not equal to sebastian"))
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
    Assert
      .assert(that(Some(" Sebastian")).wouldBeEqualTo("sebastian").otherwise("Sebastian is not equal to sebastian"))
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
    Assert
      .assert(that(Some("Sebastian ")).wouldBeEqualTo("sebastian").otherwise("Sebastian is not equal to sebastian"))
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
  }

  it should "wouldBeEqualTo variable" in {
    val customer = Customer(Some("Sebastian"), None, None, None)
    Assert
      .assert(that({ aCustomer:Customer => aCustomer.name}).wouldBeEqualTo("Sebastian").otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({ aCustomer:Customer => aCustomer.name}).wouldBeEqualTo("sebastian").otherwise("Sebastian is not equal to sebastian"))
      .in(customer)
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
  }

  it should "wouldBeEqualToIgnoringCase constant" in {
    Assert
      .assert(that(None).wouldBeEqualToIgnoringCase("Sebastian").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("Sebastian")).wouldBeEqualToIgnoringCase("Sebastian").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).wouldBeEqualToIgnoringCase("SEBASTIAN").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("SEBASTIAN")).wouldBeEqualToIgnoringCase("sebastian").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some(" Sebastian")).wouldBeEqualToIgnoringCase("sebastian").otherwise(" Sebastian is not equal to sebastian"))
      .expectsToBeFalseWith(" Sebastian is not equal to sebastian")
    Assert
      .assert(that(Some("Sebastian ")).wouldBeEqualToIgnoringCase("sebastian").otherwise("Sebastian  is not equal to sebastian"))
      .expectsToBeFalseWith("Sebastian  is not equal to sebastian")
    Assert
      .assert(that(Some("Sebastian")).wouldBeEqualToIgnoringCase(" Sebastian").otherwise("Sebastian is not equal to  sebastian"))
      .expectsToBeFalseWith("Sebastian is not equal to  sebastian")
    Assert
      .assert(that(Some("Sebastian")).wouldBeEqualToIgnoringCase("Sebastian ").otherwise("Sebastian is not equal to sebastian "))
      .expectsToBeFalseWith("Sebastian is not equal to sebastian ")
  }

  it should "wouldBeEqualToIgnoringCase variable" in {
    val customer = Customer(Some("sebastian"), None, None, None)
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldBeEqualToIgnoringCase("SEBASTIAN").otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldBeEqualToIgnoringCase("SEBASTIAN ").otherwise("Sebastian is not equal to sebastian "))
      .in(customer)
      .expectsToBeFalseWith("Sebastian is not equal to sebastian ")
  }

  it should "wouldStartWith constant" in {
    Assert
      .assert(that(None).wouldStartWith("Sebas").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("Sebastian")).wouldStartWith("Sebas").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("Sebastian")).wouldStartWith("sebas").otherwise("Sebastian does not start with sebas"))
      .expectsToBeFalseWith("Sebastian does not start with sebas")
    Assert
      .assert(that(Some("Sebastian")).wouldStartWith(" Sebas").otherwise("Sebastian does not start with  Sebas"))
      .expectsToBeFalseWith("Sebastian does not start with  Sebas")
    Assert
      .assert(that(Some("Sebastian")).wouldStartWith("Sebas ").otherwise("Sebastian does not start with Sebas "))
      .expectsToBeFalseWith("Sebastian does not start with Sebas ")
  }

  it should "wouldStartWith variable" in {
    val customer = Customer(Some("sebastian"), None, None, None)
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldStartWith("sebas").otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldStartWith("SEBAS").otherwise("sebastian does not start with SEBAS"))
      .in(customer)
      .expectsToBeFalseWith("sebastian does not start with SEBAS")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldStartWith("sebas ").otherwise("sebastian does not start with sebas "))
      .in(customer)
      .expectsToBeFalseWith("sebastian does not start with sebas ")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldStartWith(" sebas").otherwise("sebastian does not start with  sebas"))
      .in(customer)
      .expectsToBeFalseWith("sebastian does not start with  sebas")
  }

  it should "wouldStartWithIgnoringCase constant" in {
    Assert
      .assert(that(None).wouldStartWithIgnoringCase("SEBAS").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).wouldStartWithIgnoringCase("SEBAS").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("SEBASTIAN")).wouldStartWithIgnoringCase("sebas").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).wouldStartWithIgnoringCase("sebas ").otherwise("sebastian does not start with sebas "))
      .expectsToBeFalseWith("sebastian does not start with sebas ")
    Assert
      .assert(that(Some("sebastian")).wouldStartWithIgnoringCase(" sebas").otherwise("sebastian does not start with  sebas"))
      .expectsToBeFalseWith("sebastian does not start with  sebas")
  }

  it should "wouldStartWithIgnoringCase variable" in {
    val customer = Customer(Some("sebastian"), None, None, None)
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldStartWithIgnoringCase("SEBAS").otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldStartWithIgnoringCase("sebas ").otherwise("sebastian does not start with sebas "))
      .in(customer)
      .expectsToBeFalseWith("sebastian does not start with sebas ")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldStartWithIgnoringCase(" sebas").otherwise("sebastian does not start with  sebas"))
      .in(customer)
      .expectsToBeFalseWith("sebastian does not start with  sebas")
    val anotherCustomer = Customer(Some("SEBASTIAN"), None, None, None)
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldStartWithIgnoringCase("sebas").otherwise("No way it fails..."))
      .in(anotherCustomer)
      .expectsToBeTrue()
  }

  it should "wouldEndWith constant" in {
    Assert
      .assert(that(None).wouldEndWith("tian").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("Sebastian")).wouldEndWith("tian").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("Sebastian")).wouldEndWith("TIAN").otherwise("Sebastian does not end with TIAN"))
      .expectsToBeFalseWith("Sebastian does not end with TIAN")
    Assert
      .assert(that(Some("Sebastian")).wouldEndWith(" tian").otherwise("Sebastian does not end with  tian"))
      .expectsToBeFalseWith("Sebastian does not end with  tian")
    Assert
      .assert(that(Some("Sebastian")).wouldEndWith("tian ").otherwise("Sebastian does not end with tian "))
      .expectsToBeFalseWith("Sebastian does not end with tian ")
  }

  it should "wouldEndWith variable" in {
    val customer = Customer(Some("sebastian"), None, None, None)
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldEndWith("tian").otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldEndWith("TIAN").otherwise("sebastian does not end with TIAN"))
      .in(customer)
      .expectsToBeFalseWith("sebastian does not end with TIAN")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldEndWith("tian ").otherwise("sebastian does not end with tian "))
      .in(customer)
      .expectsToBeFalseWith("sebastian does not end with tian ")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldEndWith(" tian").otherwise("sebastian does not end with  tian"))
      .in(customer)
      .expectsToBeFalseWith("sebastian does not end with  tian")
  }

  it should "wouldEndWithIgnoringCase constant" in {
    Assert
      .assert(that(None).wouldEndWithIgnoringCase("TIAN").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).wouldEndWithIgnoringCase("TIAN").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("SEBASTIAN")).wouldEndWithIgnoringCase("tian").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).wouldEndWithIgnoringCase("tian ").otherwise("sebastian does not end with tian "))
      .expectsToBeFalseWith("sebastian does not end with tian ")
    Assert
      .assert(that(Some("sebastian")).wouldEndWithIgnoringCase(" tian").otherwise("sebastian does not end with  tian"))
      .expectsToBeFalseWith("sebastian does not end with  tian")
  }

  it should "wouldEndWithIgnoringCase variable" in {
    val customer = Customer(Some("sebastian"), None, None, None)
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldEndWithIgnoringCase("TIAN").otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldEndWithIgnoringCase("tian ").otherwise("sebastian does not end with tian "))
      .in(customer)
      .expectsToBeFalseWith("sebastian does not end with tian ")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldEndWithIgnoringCase(" tian").otherwise("sebastian does not end with  tian"))
      .in(customer)
      .expectsToBeFalseWith("sebastian does not end with  tian")
    val anotherCustomer = Customer(Some("SEBASTIAN"), None, None, None)
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldEndWithIgnoringCase("tian").otherwise("No way it fails..."))
      .in(anotherCustomer)
      .expectsToBeTrue()
  }

  it should "wouldContain constant" in {
    Assert
      .assert(that(None).wouldContain("basti").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("Sebastian")).wouldContain("basti").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("Sebastian")).wouldContain("BASTI").otherwise("Sebastian does not contain BASTI"))
      .expectsToBeFalseWith("Sebastian does not contain BASTI")
    Assert
      .assert(that(Some("Sebastian")).wouldContain(" basti").otherwise("Sebastian does not contain  basti"))
      .expectsToBeFalseWith("Sebastian does not contain  basti")
    Assert
      .assert(that(Some("Sebastian")).wouldContain("basti ").otherwise("Sebastian does not contain basti "))
      .expectsToBeFalseWith("Sebastian does not contain basti ")
  }

  it should "wouldContain variable" in {
    val customer = Customer(Some("Sebastian"), None, None, None)
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldContain("basti").otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldContain("BASTI").otherwise("Sebastian does not contain BASTI"))
      .in(customer)
      .expectsToBeFalseWith("Sebastian does not contain BASTI")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldContain(" basti").otherwise("Sebastian does not contain  basti"))
      .in(customer)
      .expectsToBeFalseWith("Sebastian does not contain  basti")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldContain("basti ").otherwise("Sebastian does not contain basti "))
      .in(customer)
      .expectsToBeFalseWith("Sebastian does not contain basti ")
  }

  it should "wouldContainIgnoringCase constant" in {
    Assert
      .assert(that(None).wouldContainIgnoringCase("basti").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("SEBASTIAN")).wouldContainIgnoringCase("basti").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("Sebastian")).wouldContainIgnoringCase("BASTI").otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("Sebastian")).wouldContainIgnoringCase(" basti").otherwise("Sebastian does not contain  basti"))
      .expectsToBeFalseWith("Sebastian does not contain  basti")
    Assert
      .assert(that(Some("Sebastian")).wouldContainIgnoringCase("basti ").otherwise("Sebastian does not contain basti "))
      .expectsToBeFalseWith("Sebastian does not contain basti ")
  }

  it should "wouldContainIgnoringCase variable" in {
    val customer = Customer(Some("Sebastian"), None, None, None)
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldContainIgnoringCase("basti").otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldContainIgnoringCase(" basti").otherwise("Sebastian does not contain  basti"))
      .in(customer)
      .expectsToBeFalseWith("Sebastian does not contain  basti")
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldContainIgnoringCase("basti ").otherwise("Sebastian does not contain basti "))
      .in(customer)
      .expectsToBeFalseWith("Sebastian does not contain basti ")
    val anotherCustomer = Customer(Some("SEBASTIAN"), None, None, None)
    Assert
      .assert(that({aCustomer:Customer => aCustomer.name}).wouldContainIgnoringCase("basti").otherwise("No way it fails..."))
      .in(anotherCustomer)
      .expectsToBeTrue()
  }

  it should "wouldBeEmail constant" in {
    Assert
      .assert(that(None).wouldBeEmail.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("user@domain.com")).wouldBeEmail.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("user@domain.co.in")).wouldBeEmail.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("user.name@domain.com")).wouldBeEmail.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("user_name@domain.com")).wouldBeEmail.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("username@yahoo.corporate.in")).wouldBeEmail.otherwise("No way it fails..."))
      .expectsToBeTrue()

    Assert
      .assert(that(Some("")).wouldBeEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
    Assert
      .assert(that(Some("@yahoo.com")).wouldBeEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
    Assert
      .assert(that(Some("username@")).wouldBeEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
    Assert
      .assert(that(Some(".username@yahoo.com")).wouldBeEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
    Assert
      .assert(that(Some("username@yahoo.com.")).wouldBeEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
    Assert
      .assert(that(Some("username@yahoo..com")).wouldBeEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
    Assert
      .assert(that(Some("username@yahoo.c")).wouldBeEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
    Assert
      .assert(that(Some("username@yahoo.corporate")).wouldBeEmail.otherwise("malformed email"))
      .expectsToBeFalseWith("malformed email")
  }

  it should "wouldBeEmail variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), None, None)
    Assert
      .assert(that({customer:Customer => customer.email}).wouldBeEmail.otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()

    val anotherCustomer = Customer(Some("david"), Some("david@email.c"), None, None)
    Assert
      .assert(that({customer:Customer => customer.email}).wouldBeEmail.otherwise("malformed email"))
      .in(anotherCustomer)
      .expectsToBeFalseWith("malformed email")
  }

  it should "wouldBeUri constant" in {
    Assert
      .assert(that(None).wouldBeUri.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("http://google.com")).wouldBeUri.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("http://google.com.ar")).wouldBeUri.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("www.google.com")).wouldBeUri.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("https://google.com")).wouldBeUri.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("http://www.google.com")).wouldBeUri.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("http://goo gle.com")).wouldBeUri.otherwise("malformed uri"))
      .expectsToBeFalseWith("malformed uri")
    Assert
      .assert(that(Some("http://google.com.ar.")).wouldBeUri.otherwise("malformed uri"))
      .expectsToBeFalseWith("malformed uri")
    Assert
      .assert(that(Some("http://goo$gle.com.ar")).wouldBeUri.otherwise("malformed uri"))
      .expectsToBeFalseWith("malformed uri")
  }

  it should "wouldBeUri variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.mystory.com"), None)
    Assert
      .assert(that({customer:Customer => customer.homepage}).wouldBeUri.otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer(Some("david"), Some("david@email.com"), Some("www.mys$tory.com"), None)
    Assert
      .assert(that({customer:Customer => customer.homepage}).wouldBeUri.otherwise("malformed uri"))
      .in(anotherCustomer)
      .expectsToBeFalseWith("malformed uri")
  }

  it should "wouldBeAlphanumeric constant" in {
    Assert
      .assert(that(None).wouldBeAlphanumeric.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("abc123")).wouldBeAlphanumeric.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("abc")).wouldBeAlphanumeric.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("123")).wouldBeAlphanumeric.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("abc$123")).wouldBeAlphanumeric.otherwise("not alphanumeric"))
      .expectsToBeFalseWith("not alphanumeric")
    Assert
      .assert(that(Some("abc 123")).wouldBeAlphanumeric.otherwise("not alphanumeric"))
      .expectsToBeFalseWith("not alphanumeric")
    Assert
      .assert(that(Some("abc.123")).wouldBeAlphanumeric.otherwise("not alphanumeric"))
      .expectsToBeFalseWith("not alphanumeric")
    Assert
      .assert(that(Some("")).wouldBeAlphanumeric.otherwise("not alphanumeric"))
      .expectsToBeFalseWith("not alphanumeric")
  }

  it should "wouldBeAlphanumeric variable" in {
    val customer = Customer(Some("sebastian123"), Some("sebastian@email.com"), Some("www.google.com"), None)
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeAlphanumeric.otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer(Some("seb$astian"), Some("sebastian@email.com"), Some("www.google.com"), None)
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeAlphanumeric.otherwise("not alphanumeric"))
      .in(anotherCustomer)
      .expectsToBeFalseWith("not alphanumeric")
  }

  it should "wouldBeAlphabetic constant" in {
    Assert
      .assert(that(None).wouldBeAlphabetic.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("abc")).wouldBeAlphabetic.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("ABC")).wouldBeAlphabetic.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("xyz")).wouldBeAlphabetic.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("XYZ")).wouldBeAlphabetic.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("")).wouldBeAlphabetic.otherwise("not alphabetic"))
      .expectsToBeFalseWith("not alphabetic")
    Assert
      .assert(that(Some("123")).wouldBeAlphabetic.otherwise("not alphabetic"))
      .expectsToBeFalseWith("not alphabetic")
    Assert
      .assert(that(Some("$")).wouldBeAlphabetic.otherwise("not alphabetic"))
      .expectsToBeFalseWith("not alphabetic")
    Assert
      .assert(that(Some("ab%c")).wouldBeAlphabetic.otherwise("not alphabetic"))
      .expectsToBeFalseWith("not alphabetic")
    Assert
      .assert(that(Some("ab1c")).wouldBeAlphabetic.otherwise("not alphabetic"))
      .expectsToBeFalseWith("not alphabetic")
  }

  it should "wouldBeAlphabetic variant" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@gmail.com"), Some("www.sebastian.com"), None)
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeAlphabetic.otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer(Some("dav3e"), Some("dave@gmail.com"), Some("www.dave.com"), None)
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeAlphabetic.otherwise("not alphabetic"))
      .in(anotherCustomer)
      .expectsToBeFalseWith("not alphabetic")
  }

  it should "wouldBeNumber constant" in {
    Assert
      .assert(that(None).wouldBeNumber.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("123")).wouldBeNumber.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("")).wouldBeNumber.otherwise("not a number"))
      .expectsToBeFalseWith("not a number")
    Assert
      .assert(that(Some("123 ")).wouldBeNumber.otherwise("not a number"))
      .expectsToBeFalseWith("not a number")
    Assert
      .assert(that(Some("12$3")).wouldBeNumber.otherwise("not a number"))
      .expectsToBeFalseWith("not a number")
    Assert
      .assert(that(Some("12a3")).wouldBeNumber.otherwise("not a number"))
      .expectsToBeFalseWith("not a number")
  }

  it should "wouldBeNumber variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
    Assert
      .assert(that({customer:Customer => customer.age}).wouldBeNumber.otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer(Some("dave"), Some("dave@email.com"), Some("www.dave.com"), Some("3a7"))
    Assert
      .assert(that({customer:Customer => customer.age}).wouldBeNumber.otherwise("not a number"))
      .in(anotherCustomer)
      .expectsToBeFalseWith("not a number")
  }

  it should "wouldBeSameLengthAs constant" in {
    Assert
      .assert(that(None).wouldBeSameLengthAs(9).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).wouldBeSameLengthAs(9).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).wouldBeSameLengthAs(8).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
    Assert
      .assert(that(Some("sebastian")).wouldBeSameLengthAs(10).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
  }

  it should "wouldBeSameLengthAs variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeSameLengthAs(9).otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeSameLengthAs(8).otherwise("boom!"))
      .in(customer)
      .expectsToBeFalseWith("boom!")
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeSameLengthAs(10).otherwise("boom!"))
      .in(customer)
      .expectsToBeFalseWith("boom!")
  }

  it should "wouldBeLongerThan constant" in {
    Assert
      .assert(that(None).wouldBeLongerThan(8).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).wouldBeLongerThan(8).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).wouldBeLongerThan(9).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
    Assert
      .assert(that(Some("sebastian")).wouldBeLongerThan(10).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
  }

  it should "wouldBeLongerThan variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeLongerThan(8).otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeLongerThan(9).otherwise("boom!"))
      .in(customer)
      .expectsToBeFalseWith("boom!")
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeLongerThan(10).otherwise("boom!"))
      .in(customer)
      .expectsToBeFalseWith("boom!")
  }

  it should "wouldBeLongerThanOrEqualTo constant" in {
    Assert
      .assert(that(None).wouldBeLongerThanOrEqualTo(8).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).wouldBeLongerThanOrEqualTo(8).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).wouldBeLongerThanOrEqualTo(9).otherwise("boom!"))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).wouldBeLongerThanOrEqualTo(10).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
  }

  it should "wouldBeLongerThanOrEqualTo variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeLongerThanOrEqualTo(8).otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeLongerThanOrEqualTo(9).otherwise("boom!"))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeLongerThanOrEqualTo(10).otherwise("boom!"))
      .in(customer)
      .expectsToBeFalseWith("boom!")
  }

  it should "wouldBeShorterThan constant" in {
    Assert
      .assert(that(None).wouldBeShorterThan(10).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).wouldBeShorterThan(10).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).wouldBeShorterThan(9).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
    Assert
      .assert(that(Some("sebastian")).wouldBeShorterThan(8).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
  }

  it should "wouldBeShorterThan variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeShorterThan(10).otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeShorterThan(9).otherwise("boom!"))
      .in(customer)
      .expectsToBeFalseWith("boom!")
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeShorterThan(8).otherwise("boom!"))
      .in(customer)
      .expectsToBeFalseWith("boom!")
  }

  it should "wouldBeShorterThanOrEqualTo constant" in {
    Assert
      .assert(that(None).wouldBeShorterThanOrEqualTo(10).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).wouldBeShorterThanOrEqualTo(10).otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).wouldBeShorterThanOrEqualTo(9).otherwise("boom!"))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("sebastian")).wouldBeShorterThanOrEqualTo(8).otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
  }

  it should "wouldBeShorterThanOrEqualTo variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeShorterThanOrEqualTo(10).otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeShorterThanOrEqualTo(9).otherwise("boom!"))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeShorterThanOrEqualTo(8).otherwise("boom!"))
      .in(customer)
      .expectsToBeFalseWith("boom!")
  }

  it should "wouldBeBlank constant" in {
    Assert
      .assert(that(None).wouldBeBlank.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("")).wouldBeBlank.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("  ")).wouldBeBlank.otherwise("No way it fails..."))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("a")).wouldBeBlank.otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
  }

  it should "wouldBeBlank variable" in {
    val customer = Customer(Some(" "), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeBlank.otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeBlank.otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
  }

  it should "wouldBeNotBlank constant" in {
    Assert
      .assert(that(None).wouldBeNotBlank.otherwise("boom!"))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("a")).wouldBeNotBlank.otherwise("boom!"))
      .expectsToBeTrue()
    Assert
      .assert(that(Some("")).wouldBeNotBlank.otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
    Assert
      .assert(that(Some("  ")).wouldBeNotBlank.otherwise("boom!"))
      .expectsToBeFalseWith("boom!")
  }

  it should "wouldBeNotBlank variable" in {
    val customer = Customer(Some("sebastian"), Some("sebastian@email.com"), Some("www.sebastian.com"), Some("37"))
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeNotBlank.otherwise("boom!"))
      .in(customer)
      .expectsToBeTrue()
    val anotherCustomer = Customer(Some("  "), Some("dave@email.com"), Some("www.dave.com"), Some("35"))
    Assert
      .assert(that({customer:Customer => customer.name}).wouldBeNotBlank.otherwise("boom!"))
      .in(anotherCustomer)
      .expectsToBeFalseWith("boom!")
  }
}
