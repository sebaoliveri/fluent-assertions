import assertion.Assert
import org.scalatest.{FlatSpec, Matchers}
import assertion.Assertion._


class ObjectAssertionSpec extends FlatSpec with Matchers {

  case class CustomerContext(customer: Customer)

  case class Customer(name: String, email: Option[String], phone: Option[String]) {
    def hasEitherEmailOrPhone: Boolean =
      (email, phone) match {
        case (Some(_),Some(_)) => true
        case (Some(_),None) => true
        case (None,Some(_)) => true
        case _ => false
      }
  }

  it should "isTrueThat constant" in {
    Assert
      .assert(thatFor(Customer("sebastian", Some("sebastian@email.com"), None)).isTrueThat({ customer:Customer => customer.hasEitherEmailOrPhone }).otherwise("Boom!"))
      .expectsToBeTrue()
    Assert
      .assert(thatFor(Customer("dave", None, Some("48998765"))).isTrueThat({ customer:Customer => customer.hasEitherEmailOrPhone}).otherwise("Boom!"))
      .expectsToBeTrue()
    Assert
      .assert(thatFor(Customer("juli", None, None)).isTrueThat({ customer:Customer => customer.hasEitherEmailOrPhone}).otherwise("Boom!"))
      .expectsToBeFalseWith("Boom!")
  }

  it should "isTrueThat variable" in {
    val context = CustomerContext(Customer("thelma", Some("thelma@email.com"), None))
    Assert
      .assert(thatFor({context:CustomerContext => context.customer}).isTrueThat({ customer:Customer => customer.hasEitherEmailOrPhone }).otherwise("Boom!"))
      .verifiedIn(context)
      .expectsToBeTrue()
    val anotherContext = CustomerContext(Customer("thelma", None, Some("237891239")))
    Assert
      .assert(thatFor({context:CustomerContext => context.customer}).isTrueThat({ customer:Customer => customer.hasEitherEmailOrPhone}).otherwise("Boom!"))
      .verifiedIn(anotherContext)
      .expectsToBeTrue()
    val yetAnotherContext = CustomerContext(Customer("thelma", None, None))
    Assert
      .assert(thatFor({context:CustomerContext => context.customer}).isTrueThat({ customer:Customer => customer.hasEitherEmailOrPhone}).otherwise("Boom!"))
      .verifiedIn(yetAnotherContext)
      .expectsToBeFalseWith("Boom!")
  }

  it should "isFalseThat constant" in {
    Assert
      .assert(thatFor(Customer("sebastian", Some("sebastian@email.com"), None)).isFalseThat({ customer:Customer => customer.hasEitherEmailOrPhone }).otherwise("Boom!"))
      .expectsToBeFalseWith("Boom!")
    Assert
      .assert(thatFor(Customer("dave", None, Some("48998765"))).isFalseThat({ customer:Customer => customer.hasEitherEmailOrPhone}).otherwise("Boom!"))
      .expectsToBeFalseWith("Boom!")
    Assert
      .assert(thatFor(Customer("juli", None, None)).isFalseThat({ customer:Customer => customer.hasEitherEmailOrPhone}).otherwise("Boom!"))
      .expectsToBeTrue()
  }

  it should "isFalseThat variable" in {
    val context = CustomerContext(Customer("sebastian", Some("sebastian@email.com"), None))
    Assert
      .assert(thatFor({context:CustomerContext => context.customer}).isFalseThat({ customer:Customer => customer.hasEitherEmailOrPhone }).otherwise("Boom!"))
      .verifiedIn(context)
      .expectsToBeFalseWith("Boom!")
    val anotherContext = CustomerContext(Customer("sebastian", None, Some("48998765")))
    Assert
      .assert(thatFor({context:CustomerContext => context.customer}).isFalseThat({ customer:Customer => customer.hasEitherEmailOrPhone}).otherwise("Boom!"))
      .verifiedIn(anotherContext)
      .expectsToBeFalseWith("Boom!")
    val yetAnotherContext = CustomerContext(Customer("sebastian", None, None))
    Assert
      .assert(thatFor({context:CustomerContext => context.customer}).isFalseThat({ customer:Customer => customer.hasEitherEmailOrPhone}).otherwise("Boom!"))
      .verifiedIn(yetAnotherContext)
      .expectsToBeTrue()
  }

  it should "and" in {
    Assert
      .assert(
        thatFor(Customer("sebastian", Some("sebastian@email.com"), None)).isTrueThat({ customer:Customer => customer.hasEitherEmailOrPhone })
        .andThat("sebastian").startsWith("seb").otherwise("Boom!"))
      .expectsToBeTrue()
    Assert
      .assert(
        thatFor(Customer("sebastian", Some("sebastian@email.com"), None)).isTrueThat({ customer:Customer => customer.hasEitherEmailOrPhone })
        .andThat("sebastian").startsWith("eba").otherwise("Boom!"))
      .expectsToBeFalseWith("Boom!")
    Assert
      .assert(
        thatFor(Customer("juli", None, None)).isTrueThat({ customer:Customer => customer.hasEitherEmailOrPhone})
        .andThat("sebastian").startsWith("seba").otherwise("Boom!"))
      .expectsToBeFalseWith("Boom!")
    Assert
      .assert(
        thatFor(Customer("juli", None, None)).isTrueThat({ customer:Customer => customer.hasEitherEmailOrPhone})
        .andThat("sebastian").startsWith("eba").otherwise("Boom!"))
      .expectsToBeFalseWith("Boom!")
  }

  it should "or" in {
    Assert
      .assert(
        thatFor(Customer("sebastian", Some("sebastian@email.com"), None)).isTrueThat({ customer:Customer => customer.hasEitherEmailOrPhone })
          .orThat("sebastian").startsWith("seb").otherwise("Boom!"))
      .expectsToBeTrue()
    Assert
      .assert(
        thatFor(Customer("sebastian", Some("sebastian@email.com"), None)).isTrueThat({ customer:Customer => customer.hasEitherEmailOrPhone })
          .orThat("sebastian").startsWith("eba").otherwise("Boom!"))
      .expectsToBeTrue()
    Assert
      .assert(
        thatFor(Customer("juli", None, None)).isTrueThat({ customer:Customer => customer.hasEitherEmailOrPhone})
          .orThat("sebastian").startsWith("seba").otherwise("Boom!"))
      .expectsToBeTrue()
    Assert
      .assert(
        thatFor(Customer("juli", None, None)).isTrueThat({ customer:Customer => customer.hasEitherEmailOrPhone})
          .orThat("sebastian").startsWith("eba").otherwise("Boom!"))
      .expectsToBeFalseWith("Boom!")
  }
}
