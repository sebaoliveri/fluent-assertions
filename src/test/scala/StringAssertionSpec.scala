import assertion.Assert
import org.scalatest.{FlatSpec, Matchers}
import assertion.Assertion._

class StringAssertionSpec extends FlatSpec with Matchers {

  case class Customer(name: String)

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
    val customer = Customer("Sebastian")
    Assert
      .assert(that({ aCustomer:Customer => aCustomer.name}).isEqualTo("Sebastian").otherwise("No way it fails..."))
      .in(customer)
      .expectsToBeTrue()
    Assert
      .assert(that({ aCustomer:Customer => aCustomer.name}).isEqualTo("sebastian").otherwise("Sebastian is not equal to sebastian"))
      .in(customer)
      .expectsToBeFalseWith("Sebastian is not equal to sebastian")
  }
}
