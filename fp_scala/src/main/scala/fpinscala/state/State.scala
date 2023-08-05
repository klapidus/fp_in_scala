package fpinscala.state

trait RNG:
  def nextInt: (Int, RNG)


case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

val rng = SimpleRNG(2128506)
val (n, rng2) = rng.nextInt

def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (n, rng2) = rng.nextInt
  //if (n < 0) (-n, rng2)
  //else (n, rng2)
  // RB solution
  (if n < 0 then -(n + 1) else n, rng2)
}

def double(rng: RNG): (Double, RNG) = {
  val (nInt, rng2) = nonNegativeInt(rng)
  val d = ((nInt - 1)/Int.MaxValue).toDouble
  (d, rng2)
}

def pair1(rng: RNG): ((Double, Int, Double), RNG) = {
  val (d1, rng1) = double(rng)
  val (i2, rng2) = nonNegativeInt(rng1)
  val (d3, rng3) = double(rng2)
  ((d1, i2, d3), rng3)
}

def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
  val (n, rng_next) = nonNegativeInt(rng)
  if count == 0 then (Nil, rng_next)
  else (n :: ints(count - 1)(rng_next)._1, rng_next)
}

//val rng7 = SimpleRNG(seed=108)
//ints(3)(rng7)


type Rand[+A] = RNG => (A, RNG)

def unit[A](a: A): Rand[A] = {
  rng => (a, rng)
}

def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
  // nothing but a function: RNG => (B, RNG)
  rng => {
    val (a, rng1) = s(rng)
    (f(a), rng1)
  }
}

def doubleViaMap(rng: RNG): (Double, RNG) = {
  map(nonNegativeInt){
    (x: Int) => ((x - 1)/Int.MaxValue).toDouble
  }(rng)
}

// note the difference
// in the first case the function returns a value (Double, RNG)
// in the second case the function returns a value
// Rand[Double], that is RNG => (Double, RNG)
def doubleViaMapAction: Rand[Double] = {
  map(nonNegativeInt){
    (x: Int) => ((x - 1)/Int.MaxValue).toDouble
  }
}

//doubleViaMap(rng)

def map2[A, B, C](ra: Rand[A])(rb: Rand[B])
                 (f: (A, B) => C): Rand[C] = {
  rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }
}

def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
  map2(ra)(rb)((_, _))

def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = {
  // reminder: what we need to construct is RNG => (B, RNG)
  rng => {
    val (a, rngNext) = r(rng)
    f(a)(rngNext)
  }
}

def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] = {
  flatMap(r){
    (x: A) => unit(f(x))
  }
}

// TODO
def map2ViaFlatMap[A, B, C](ra: Rand[A])(rb: Rand[B])
                           (f: (A, B) => C): Rand[C] = {
  flatMap(ra){
    x => {
      mapViaFlatMap(rb){
        y => f(x, y)
      }
    }
  }
}

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s => {
        val (a, s1) = underlying(s)
        f(a)(s1)
      }

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] =
    s => (a, s)



