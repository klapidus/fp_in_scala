package fpinscala.testing

import fpinscala.state.{State, RNG}

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

//opaque type Prop = TestCases => Option[(FailedCase, SuccessCount)]
// in this case, however, the success will be None
// which is sort of weird
enum Result:
  case Passed
  case Falsified(failure: FailedCase, successes: SuccessCount)

  def isFalsified: Boolean = this match
    case Passed => false
    case Falsified(_, _) => true

opaque type Prop = (TestCases, RNG) => Result

def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
  (n, rng) =>
    randomLazyList(as)(rng)
      .zip(LazyList.from(0))
      .take(n)
      .map:
        case (a, i) =>
          try
            if f(a) then Result.Passed
            else Result.Falsified(a.toString, i)
          catch
            case e: Exception =>
              Result.Falsified(buildMsg(a, e), i)
      .find(_.isFalsified)
      .getOrElse(Result.Passed)

def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
  // simply creates (unfolds) an infinite Lazy List
  // of random values of type A
  LazyList.unfold(rng)(rng => Some(g.run(rng)))

def buildMsg[A](s: A, e: Exception): String =
  s"test case: $s\n" +
  s"generated an exception: ${e.getMessage}\n" +
  s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

extension (self: Prop) def &&(that: Prop): Prop =
  (n, rng) => self(n, rng) match
    case Result.Passed => that(n, rng)
    case x => x

extension (self: Prop) def ||(that: Prop): Prop =
  (n, rng) => self(n, rng) match
    case Result.Falsified(_, _) => that(n, rng)
    case x => x

