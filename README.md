## Fluent assertions

__fluent-assertions__ _is the result of materializing my motivation to propose a validation model to reify assertions as first class objects._ 

### Installation

This lib supports Scala 2.13
Add in your build.sbt the following lines:
```
resolvers += Resolver.bintrayRepo("fluent-assertions", "releases")
libraryDependencies += "nulluncertainty" %% "fluent-assertions" % "1.0.0"
```

### Usages

Raise an _AssertionFailureException_ describing all the errors if User can not be instantiated because of invalid construction: 

```scala
    import org.nulluncertainty.assertion.AssertionBuilder._

    assertThat(name).isNotBlank.otherwise("The name is required")
    .and(assertThat(name).isAlphabetic.otherwise("The name must contain alphabetic chars only"))
    .and(assertThat(email).isNotBlank.otherwise("The email is required"))
    .and(assertThat(email).isEmail.otherwise("The email is not valid"))
    .and(assertThat(password).isNotBlank.otherwise("The password is expected"))
    .and(assertThat(password).isLongerThanOrEqualTo(7).isShorterThanOrEqualTo(20).otherwise("Password length must be between 7 and 20"))
    .signalIfFailed()

```

Or raise your own _DomainValidationException_ describing all the errors if User can not be instantiated because of invalid construction: 

```scala
    import org.nulluncertainty.assertion.AssertionBuilder._

    assertThat(name).isNotBlank.otherwise("The name is required")
    .and(assertThat(name).isAlphabetic.otherwise("The name must contain alphabetic chars only"))
    .and(assertThat(email).isNotBlank.otherwise("The email is required"))
    .and(assertThat(email).isEmail.otherwise("The email is not valid"))
    .and(assertThat(password).isNotBlank.otherwise("The password is expected"))
    .and(assertThat(password).isLongerThanOrEqualTo(7).isShorterThanOrEqualTo(20).otherwise("Password length must be between 7 and 20"))
    .signalIfFailed(errors => new DomainValidationException(errors.mkString(", ")))
  
```

Assertions can also be lazily evaluated passing in a context:

```scala
    import org.nulluncertainty.assertion.AssertionBuilder._

    assertThat({user:User => user.name}).isNotBlank.otherwise("The name is required")
    .and(assertThat({user:User => user.name}).isAlphabetic.otherwise("The name must contain alphabetic chars only"))
    .and(assertThat({user:User => user.email}).isNotBlank.otherwise("The email is required"))
    .and(assertThat({user:User => user.email}).isEmail.otherwise("The email is not valid"))
    .and(assertThat({user:User => user.password}).isNotBlank.otherwise("The password is expected"))
    .and(assertThat({user:User => user.password}).isLongerThanOrEqualTo(7).isShorterThanOrEqualTo(20).otherwise("Password length must be between 7 and 20"))
    .evaluate(User("sebastian", "oliveri", "a1b2c3$"))
    .signalIfFailed()

```

Because of assertions can be lazily evaluated, at the end it is just a description of what should be evaluated:

```scala
    import org.nulluncertainty.assertion.AssertionBuilder._

    val userValidation =
        assertThat({user:User => user.name}).isNotBlank.otherwise("The name is required")
        .and(assertThat({user:User => user.name}).isAlphabetic.otherwise("The name must contain alphabetic chars only"))
        .and(assertThat({user:User => user.email}).isNotBlank.otherwise("The email is required"))
        .and(assertThat({user:User => user.email}).isEmail.otherwise("The email is not valid"))
        .and(assertThat({user:User => user.password}).isNotBlank.otherwise("The password is expected"))
        .and(assertThat({user:User => user.password}).isLongerThanOrEqualTo(7).isShorterThanOrEqualTo(20).otherwise("Password length must be between 7 and 20"))

    userValidation
        .evaluate(User("sebastian", "oliveri", "a1b2c3$"))
        .signalIfFailed()

```

Just the only one instance of _userValidation_ can be used for validating all the requests. 

Assertions can be composed with __and__ and __or__ operators.

Here we expect the user to fill a user registration form fulfilling certain business criteria:

```scala

    case class UserRegistrationForm(maybeEmail: Option[String], maybePhoneNumber: Option[String], password: String)

    val userRegistrationForm = UserRegistrationForm(maybeEmail = Some("sebastian@gmail.com"), maybePhoneNumber = None,  "sj28d$oU9%u")

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

Given the assertion previously written you can pattern match:

```scala

    eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword
      .evaluate(UserRegistrationForm(Some("sebasmail@gmail.com"), None, "1a2b3c$"))
      .matches {
        case AssertionSuccessfulResult(userRegistrationForm) =>
        case AssertionFailureResult(errors) =>
      }

```

Another possibility is to get a Try[T] out of it

```scala

    eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword
      .evaluate(userRegistrationForm)
      .toTry()
      .map(context => /* keep going */)
      .recover { case AssertionFailureException(errors) => /* keep going */ }

``` 

or to get an Either[AssertionFailureException, T]

```scala

    eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword
      .evaluate(userRegistrationForm)
      .toEither()
      .map(context => /* keep going */)
      .recover { case AssertionFailureException(errors) => /* keep going */ }

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

They can also be used in test cases, expected to be True:


```scala
    import org.nulluncertainty.assertion.AssertionBuilder._

    assertThat(name).isNotBlank.otherwise("The name is required")
    .and(assertThat(name).isAlphabetic.otherwise("The name must contain alphabetic chars only"))
    .and(assertThat(email).isNotBlank.otherwise("The email is required"))
    .and(assertThat(email).isEmail.otherwise("The email is not valid"))
    .and(assertThat(password).isNotBlank.otherwise("The password is expected"))
    .and(assertThat(password).isLongerThanOrEqualTo(7).isShorterThanOrEqualTo(20).otherwise("Password length must be between 7 and 20"))
    .expectsToBeTrue()

```
