package fpinscala.testing

opaque type FailedCase = String
opaque type SuccessCount = Int

//trait Prop:
//  def check: Either[(FailedCase, SuccessCount), SuccessCount]

opaque type TestCases = Int

object TestCases:
  extension (x: TestCases) def toInt: Int = x
  def fromInt(x: Int): TestCases = x

//opaque type Prop = TestCases =>
//  Either[(FailedCase, SuccessCount), SuccessCount]
// we don't gain anything on the right side of Either
// it is simply TestCases: Int (number of successful tests)

opaque type Prop = TestCases => Option[(FailedCase, SuccessCount)]
// in this case, however, the success will be None
// which is sort of weird
enum Result:
  case Passed
  case Falsified(failure: FailedCase, successes: SuccessCount)

  def isFalsified: Boolean = this match
    case Passed => false
    case Falsified(_, _) => true