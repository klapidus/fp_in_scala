/*
def sum(ints: IndexedSeq[Int]): Par[Int] =
  if ints.size <= 1 then
    // note headOption here
    Par.unit(ints.headOption.getOrElse(0))
  else
    val (l, r) = ints.splitAt(ints.size / 2)
    Par.map2(sum(l), sum(r))(_ + _)
 */
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

  extension [A](pa: Par[A])
    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      es =>
        val futureA = pa(es)
        val futureB = pb(es)
        UnitFuture(f(futureA.get, futureB.get))




