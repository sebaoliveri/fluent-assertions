## Fluent assertions

A long long time ago I was taught real devs do not use IFs. However, when writing a condition using objects of different domains there's no way to avoid IFs. For Domain Driven Design we need IFs for describing preconditions. Mostly, this applies for objects construction as well as message sending between objects. Why not to validate http requests too? As you see, preconditions are key to describe scenarios for valid object instantiation and valid object transitions in any effective model.

Software models must be legible, representative of what they are modelling. The semantic gap betweeen the Model and the Reality being modelled should tend to zero. Code with tons of idented IFs-ELSEs are error prone, really hard to reason about, add accidental complexity and misrepresent the reality being modelled, among other disadvantages.  

__fluent-assertions__ _is the result of materializing my motivation to propose a validation model to reify assertions as first class objects._ 

### Installation

This lib supports Scala 2.13
Add in your build.sbt the following lines:
```
resolvers += Resolver.bintrayRepo("fluent-assertions", "releases")
libraryDependencies += "nulluncertainty" %% "fluent-assertions" % "1.0.0"
```

### Usages

A user might sign up using its email or its mobile as part of its credentials:

```scala

    case class UserRegistrationForm(maybeEmail: Option[String], maybePhoneNumber: Option[String], password: String)

```

Now, let's write an assertion to describe the required preconditions to Sign Up a user:

```scala

    import org.nulluncertainty.assertion.AssertionBuilder._

    val eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword =
      assertThat({userRegistrationForm:UserRegistrationForm => userRegistrationForm.maybeEmail})
          .isDefined
          .isEmail
        .orThat({userRegistrationForm:UserRegistrationForm => userRegistrationForm.maybePhoneNumber})
          .isDefined
          .isNumber
          .isShorterThan(20)
        .otherwise("Any of the email or the phone number must be specified")
      .and(
        assertThat({userRegistrationForm:UserRegistrationForm => userRegistrationForm.password})
          .isNotBlank
          .isLongerThan(5)
          .isShorterThanOrEqualTo(15)
          .otherwise("The password must be longer than 5 and shorter than 15"))

```

_eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword_ is an Assertion instance. Underlying it is a composition of multiple assertions.

This assertion could be written more expressive if we parametrized T:

```scala

    import org.nulluncertainty.assertion.AssertionBuilder._

    val eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword =
      assertThat[UserRegistrationForm](_.maybeEmail)
          .isDefined
          .isEmail
        .orThat(_.maybePhoneNumber)
          .isDefined
          .isNumber
          .isShorterThan(20)
        .otherwise("Any of the email or the phone number must be specified")
      .and(
        assertThat[UserRegistrationForm](_.password)
          .isNotBlank
          .isLongerThan(5)
          .isShorterThanOrEqualTo(15)
          .otherwise("The password must be longer than 5 and shorter than 15"))

```

Given:

```scala

    val userRegistrationForm = UserRegistrationForm(Some("sebasmail@gmail.com"), None, "1a2b3c$")

```

_eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword_ requires a context for evalution. The context is an instance of _UserRegistrationForm_

```scala

    eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword
      .evaluate(userRegistrationForm)
      .matches {
        case AssertionSuccessfulResult(userRegistrationForm) => // keep going
        case AssertionFailureResult(errors) => // maybe return BadRequest
      }

```

_AssertionFailureResult(errors)_ reifies the concept of a failure, and groups all the errors cause by unsatisfied assertions.

We can also select another strategy and raise an exception of type _AssertionFailureException_ which also collects all the error messages of the failed assertions after evaluation:

```scala

    eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword
      .evaluate(userRegistrationForm)
      .signalIfFailed()

```

If we need a custom exception:

```scala

    eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword
      .evaluate(userRegistrationForm)
      .signalIfFailed(errors => new DomainValidationException(errors.mkString(", ")))

```

If you need a type _Try_:

```scala

    eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword
      .evaluate(userRegistrationForm)
      .toTry()
      .map(context => /* keep going */)
      .recover { case AssertionFailureException(errors) => /* keep going */ }

```

or if you need a type _Either_:

```scala

    eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword
      .evaluate(userRegistrationForm)
      .toEither()
      .map(context => /* keep going */)
      .recover { case AssertionFailureException(errors) => /* keep going */ }

```

or if you want to _fold_ over an assertion result to return another type:

```scala

    eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword
      .evaluate(userRegistrationForm)
      .fold(errorsHandlingFunction, successFunction)
      
```

You can also use an assertion for testing.


```scala

    eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword
      .evaluate(userRegistrationForm)
      .expectsToBeTrue()
      
```

So far we saw assertions evaluated passing in a context. But we can also write assertions using values instead of functions.
In this case we assert UserRegistrationForm instantiation to be valid, otherwise raise an exception. 
Take a look at the _evaluate_ method. No context is passed in:


```scala

    import org.nulluncertainty.assertion.AssertionBuilder._
    
    case class UserRegistrationForm(maybeEmail: Option[String], maybePhoneNumber: Option[String], password: String) {
    
      assertThat(maybeEmail)
          .isDefined
          .isEmail
        .orThat(maybePhoneNumber)
          .isDefined
          .isNumber
          .isShorterThan(20)
        .otherwise("Any of the email or the phone number must be specified")
      .and(
        assertThat(password)
          .isNotBlank
          .isLongerThan(5)
          .isShorterThanOrEqualTo(15)
          .otherwise("The password must be longer than 5 and shorter than 15"))
       .evaluate()
       .signalIfFailed()
    }

```

So far we saw the example of _UserRegistrationForm_ assertion. But we can build composable assertions using any other types _number_ _string_ _date_ _collection_ _option_ _boolean_

Assertions be be composed with operators:

_ifTrue_ : When fails returns only _anAssertion_ failure message. When success, evaluates _anotherAssertion_ and returns its result.

```scala

    anAssertion.ifTrue(anotherAssertion).evaluate(context)
      
```

_and_: 

```scala

    anAssertion.and(anotherAssertion).evaluate(context)
    
```
     
_or_: 

```scala

    anAssertion.or(anotherAssertion).evaluate(context)
    
```
     

_ifFalse_: 

```scala

    anAssertion.ifFalse(anotherAssertion).evaluate(context)
      
```

_ifTrue_ _ifFalse_

```scala

    anAssertion.ifTrue(anotherAssertion).ifFalse(yetAnotherAssertion).evaluate(context)
      
```

_thenElse_ (takes a boolean expression as predicate)

```scala

    import org.nulluncertainty.extension._    

    "jony".startsWithExp("j").thenElse(anAssertion, anotherAssertion).evaluate(context)
      
```

Assertions can also be composable by using Map and FlatMap:

```scala
    import org.nulluncertainty.extension.QuantifiableExt._

    assertThat({customersByAge: Map[Int,String] => customersByAge}).containsNoDuplicates.otherwise("repeated customers not allowed")
      .map(_.keys)
      .flatMap(ages => assertThat(ages).forAll(age => age.isGreaterThanExp(18)).otherwise("all customers must be 18 years old or greater"))
      .evaluate(Map(
        38 -> "Sebastian",
        40 -> "Juan"
      )).matches {
        case AssertionSuccessfulResult(_) =>
        case AssertionFailureResult(errors) =>
      }

``` 



