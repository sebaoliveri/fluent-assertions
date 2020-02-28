## Fluent assertions

IMPORTANT! This Readme is a lil bit deprecated and describes just a few operations with this lib , will update it soon and publish version 1.0 to bintray...

### Motivation
A long time ago, I was taught that real developers do not use IF. Of course, there are times when IF statements can not be avoided and this happens when evaluating two objects of different domains. In all other cases they can be replaced by polimorfism.

__fluent-assertions__ _is the result of materializing my motivation to propose an org.validify.assertion model to reify assertions as first class objects._ 

### Simple Usage

Raise an _AssertionFailureException_ describing all the errors if User can not be instantiated because of invalid construction: 

```scala
case class User(name: String, email: String, password: String) {
    
    assert(that(name).isNotBlank.otherwise("The name is required")
      .and(that(name).isAlphabetic.otherwise("The name must contain alphabetic chars only"))
      .and(that(email).isNotBlank.otherwise("The email is required"))
      .and(that(email).isEmail.otherwise("The email is not valid"))
      .and(that(password).isNotBlank.otherwise("The password is expected"))
      .and(that(password).isLongerThanOrEqualTo(7).isShorterThanOrEqualTo(20).otherwise("Password length must be between 7 and 20")))
      .signalIfFailed()
  }
```

You can build your own assertions in your business logic from any types, passing in types of String, Numbers, Boolean, Date, Option[T], Iterable[T].

Assertions can be composed with __and__ and __or__ operators  

### Assertions can be also evaluated in a context 

Here we expect the user to fill a user registration form fulfilling certain business criteria:

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

```

Given the org.validify.assertion previously written you can pattern match:

```scala

    assert(eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword)
      .in(userRegistrationForm)
      .matches {
        case AssertionSuccessfulResult(userRegistrationForm) =>
        case AssertionFailureResult(errors) =>
      }

```

You can also signal an _AssertionFailureException_ to catch somewhere up in the stack execution flow

```scala

    assert(eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword)
      .in(userRegistrationForm)
      .signalIfFailed()

```

Another possibility is to get a Try[T] out of it

```scala

    assert(eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword)
      .in(userRegistrationForm)
      .toTry()
      .map(context => /* keep going */)
      .recover { case AssertionFailureException(errors) => /* keep going */ }

``` 

or to get an Either[AssertionFailureException, T]

```scala

    assert(eitherAnEmailOrAPhoneMustBeSpecifiedAlongWithPassword)
      .in(userRegistrationForm)
      .toEither()
      .map(context => /* keep going */)
      .recover { case AssertionFailureException(errors) => /* keep going */ }

``` 

