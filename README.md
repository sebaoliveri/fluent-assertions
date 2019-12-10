# fluent-assertions

An assertions model written in Scala to reify assertions as first class objects.

```scala
case class UserRegistrationForm(maybeEmail: Option[String], maybePhoneNumber: Option[String], password: String)

val userRegistrationForm = UserRegistrationForm(maybeEmail = Some("sebastian@gmail.com"), maybePhoneNumber = None, "sj28d$oU9%u")
```

Composable assertions with semantic meaning

```scala
    val eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword =
      that({userRegistrationForm:UserRegistrationForm => userRegistrationForm.maybeEmail})
          .isDefined
          .isEmail
        .orThat({userRegistrationForm:UserRegistrationForm => userRegistrationForm.maybePhoneNumber})
          .isDefined
          .isNumber
          .isShorterThan(20)
        .otherwise("Any of the email or the phone number must be specified")
      .and(
        that({userRegistrationForm:UserRegistrationForm => userRegistrationForm.password})
          .isNotBlank
          .isLongerThan(5)
          .isShorterThanOrEqualTo(15)
          .otherwise("The password must be longer than 5 and shorter than 15"))

```

Assertions evaluate in a given context (or no context) and informs the result. In case of failure it collects all the unsatisfied errors.

```scala
Assert
      .assert(eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword)
      .in(userRegistrationForm) match {
        case AssertionSuccessfulResult(form) =>
        case AssertionFailureResult(errors) => 
      }
```

When using fluent assertions in test cases, can be used like this

```scala
    Assert
      .assert(eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword)
      .in(userRegistrationForm)
      .expectsToBeTrue()

```

or this

```scala
Assert
      .assert(eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword)
      .in(userRegistrationForm)
      .expectsToBeFalseWith(expectedErrorMessages = Nil:_*) // a list of the expected error messages
```
