# fluent-assertions

An assertions model written in Scala to reify assertions as first class objects.


1) Using the assertion model passing in a User Registration Fom as a context

We expect the user to fill the registration form fulfilling certain business criteria

```scala

    case class UserRegistrationForm(maybeEmail: Option[String], maybePhoneNumber: Option[String], password: String)

    val userRegistrationForm = UserRegistrationForm(maybeEmail = Some("sebastian@gmail.com"), maybePhoneNumber = None,  "sj28d$oU9%u")

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

Assert
      .assert(eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword)
      .in(userRegistrationForm)
      .matches {
        case AssertionSuccessfulResult(userRegistrationForm) =>
        case AssertionFailureResult(errors) =>
      }
      
```

2) Using the assertion model to fulfill certain preconditions at object creation time

```scala

    case class UserRegistrationForm(maybeEmail: Option[String], maybePhoneNumber: Option[String], password: String) {
    
        val eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword =
          that(userRegistrationForm.maybeEmail)
              .isDefined
              .isEmail
            .orThat(userRegistrationForm.maybePhoneNumber)
              .isDefined
              .isNumber
              .isShorterThan(20)
            .otherwise("Any of the email or the phone number must be specified")
          .and(
            that(userRegistrationForm.password)
              .isNotBlank
              .isLongerThan(5)
              .isShorterThanOrEqualTo(15)
              .otherwise("The password must be longer than 5 and shorter than 15"))
              
              
      eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword.signalIfFails { errors => new  InvalidArgumentsException(errors.mkString(",")) }
  }


```
