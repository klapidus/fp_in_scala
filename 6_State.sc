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

val rng7 = SimpleRNG(seed=108)
ints(3)(rng7)
