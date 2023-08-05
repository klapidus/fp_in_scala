package fpinscala.testing

import fpinscala.state.{State, RNG, nonNegativeInt}


opaque type Gen[+A] = State[RNG, A]
// Gen[A] is State[RNG, A]
// that is RNG => (A, RNG)
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
    State.flatMap(self)(f)

extension [A](self: Gen[A])
  def run(rng: RNG): (A, RNG) =
    State.run(self)(rng)

// TODO:
//extension [A](self: Gen[A])
//  def listOfN(size: Gen[Int]): Gen[List[A]] =
//    size.flatMap()
