opaque type State[S, +A] = S => (A, S)

object State:
  // my understanding:
  // this is needed to use the opaque type State outside
  // otherwise the implementation S => (A, S) is lost
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)
  // again, this is a "dummy" method
  def apply[S, A](f: S => (A, S)): State[S, A] = f
  def unit[S, A](a: => A): State[S, A] = {
    s => (a, s)
  }
  extension [S, A, B](underlying: State[S, A])
    def map(f: A => B): State[S, B] = {
      s => {
        val (a, s_new) = underlying.run(s)
        (f(a), s_new)
      }
    }
  extension [S, A, B](underlying: State[S, A])
    def flatMap(f: A => State[S, B]): State[S, B] = {
      s => {
        val (a, s_new) = underlying.run(s)
        // Note: if we return f(a)
        // this would make the resulting return type
        // s => State[S, B], i.e. S => State[S, B]
        // what we need, however, is S => (B, S)
        // therefore, we have:
        f(a)(s_new)
      }
    }

trait RNG:
  def nextInt: (Int, RNG)

def nonNegativeInt(rng: RNG): (Int, RNG) =
  val (i, r) = rng.nextInt
  (if i < 0 then -(i + 1) else i, r)

opaque type Gen[+A] = State[RNG, A]
//object Gen:
  // it is not parametric on S, it's fixed to RNG
//  def apply[A](f: RNG => (A, RNG)): Gen[A] = f

object Prop:
  opaque type FailedCase = String
  opaque type SuccessCount = Int

import Prop.{FailedCase, SuccessCount}
trait Prop:
  def check: Either[(FailedCase, SuccessCount), SuccessCount]

// Gen[A] is State[RNG, A]
// that is RNG => (A, RNG)
import State.map
def choose(start: Int, stopExclusive: Int): Gen[Int] =
  State(rng => nonNegativeInt(rng)).map(
    n => start + n % (stopExclusive - start)
  )

def unit[A](a: => A): Gen[A] = {
  //rng => (a, rng)
  State.unit(a)
}

extension [A](self: Gen[A])
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    rng => {
      val (a, rng_next) = self(rng)
      f(a)(rng_next)
    }

// TODO:
//extension [A](self: Gen[A])
//  def listOfN(size: Gen[Int]): Gen[List[A]] =
//    size.flatMap()
