/*
def sum(ints: IndexedSeq[Int]): Par[Int] =
  if ints.size <= 1 then
    // note headOption here
    Par.unit(ints.headOption.getOrElse(0))
  else
    val (l, r) = ints.splitAt(ints.size / 2)
    Par.map2(sum(l), sum(r))(_ + _)
 */
import Par.fork

//import java.util.concurrent.ExecutorService
//import java.util.concurrent.Callable
import scala.concurrent.duration.TimeUnit

trait ExecutorService:
  def submit[A](a: Callable[A]): Future[A]

trait Callable[A]:
  def call: A

trait Future[A]:
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean

opaque type Par[A] = ExecutorService => Future[A]

extension [A](pa: Par[A]) def run(es: ExecutorService): Future[A] =
  pa(es)

object Par:
  private case class UnitFuture[A](get: A) extends Future[A]:
    // look, we put this inside constructor arguments
    //def get: A = a
    override def get(timeout: Long, unit: TimeUnit): A = get
    def isDone = true
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false

  // remember: Par[A] is a function: ExecutorService => Future
  def unit[A](a: A): Par[A] = es => UnitFuture(a)
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      // TODO: ???
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  extension [A](pa: Par[A])
    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      es =>
        val futureA = pa(es)
        val futureB = pb(es)
        UnitFuture(f(futureA.get, futureB.get))

  extension [A](pa: Par[A])
    def map[B](f: A => B): Par[B] =
      pa.map2(unit(()))((a, _) => f(a))


import Par._

def asyncF[A, B](f: A => B): A => Par[B] = {
  (a: A) => Par.lazyUnit(f(a))
}

def sortPar(parList: Par[List[Int]]): Par[List[Int]] = {
  val emptyValue: Par[Unit] = Par.unit(())
  parList.map2(emptyValue)((l, _) => l.sorted)
}

def sequence[A](pas: List[Par[A]]): Par[List[A]] =
  val z = Par.unit(List.empty[A])
  pas.foldRight(z)((pa, acc) => pa.map2(acc)(_ :: _))

// TODO: note the fork
def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = fork{
  val fbs: List[Par[B]] = as.map(asyncF(f))
  sequence(fbs)
}

def sequenceFilter[A](pas: List[Par[A]])(f: A => Boolean): Par[List[A]] =
  val z = Par.unit(List.empty[A])
  pas.foldRight(z)((pa, acc) => pa.map2(acc)(_ :: _))

def filterPar[A](pas: List[Par[A]])(pr: A => Boolean): Par[List[A]] =
  val z = Par.unit(List.empty[A])
  def filterFun(a: A, b: List[A]): List[A] = {
    if pr(a) then b
    else a :: b
  }
  pas.foldRight(z){
    (pa, acc) => pa.map2(acc)(filterFun)
  }

def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
  val pars: List[Par[A]] = as.map(asyncF(identity))
  filterPar(pars)(f)
}

//import java.util.concurrent.Executors

//class JExecutorService(val es: java.util.concurrent.ExecutorService) extends ExecutorService:
//  override def submit[A](a: Callable[A]): Future[A] = es.submit(a)

//val a = lazyUnit(42 + 1)
//val es = Executors.newFixedThreadPool(1)
//println(a(es))

def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
  es =>
    val idx = n.run(es).get
    choices(idx).run(es)

//def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A]

extension [A](pa: Par[A])
  def flatMap[B](f: A => Par[B]): Par[B] =
    es =>
      f(pa.run(es).get).run(es)

def join[A](ppa: Par[Par[A]]): Par[A] = es => {
  ppa.run(es).get.run(es)
}

extension [A](pa: Par[A])
  def flatMapViaJoin[B](f: A => Par[B]): Par[B] = {
    join(pa.map(f))
  }

def joinViaFlatMap[A](ppa: Par[Par[A]]): Par[A] =
  // honestly, didn't get this one:
  ppa.flatMap(identity)

