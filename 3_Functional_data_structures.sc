{
  enum List[+A]:
    case Nil
    case Cons(head: A, tail: List[A])

  object List:
    def apply[A](as: A*): List[A] =
      if as.isEmpty then Nil
      else Cons(as.head, apply(as.tail *))

    // 3.2
    def tail[A](l: List[A]) = l match {
      case Nil => sys.error("Empty list.")
      case Cons(_, t) => t
    }

    // 3.3
    def setHead[A](value: A, l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, t) => Cons(value, t)
    }

    // 3.4
    def drop[A](as: List[A], n: Int): List[A] = as match {
      case Nil => Nil
      case Cons(_, t) => {
        if n == 1 then t
        else drop(t, n - 1)
      }
    }

    // 3.5
    def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
      case Nil => Nil
      case Cons(h, t) => {
        if f(h) then dropWhile(t, f)
        else as
      }
    }

    // 3.6
    def initK[A](as: List[A]): List[A] = as match {
      case Cons(h, t) => t match {
        case Nil => Nil
        case Cons(_, _) => Cons(h, init(t))
      }
      case Nil => Nil
    }

    def init[A](as: List[A]): List[A] = as match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(hd, tl) => Cons(hd, init(tl))
    }

    def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B = as match {
      case Nil => acc
      case Cons(h, t) => f(h, foldRight(t, acc, f))
    }

    // 3.9
    def length[A](as: List[A]): Int =
      foldRight(as, 0: Int, (_, y) => 1 + y)

    // 3.10
    @annotation.tailrec
    def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B = as match {
      case Nil => acc // the trick is, this will be the final acc
      // this is different from foldRight where
      // case Nil => acc return the initial (input) value for acc
      case Cons(hd, tl) => foldLeft(tl, f(acc, hd), f)
    }

    // 3.11
    def sumFoldLeft(as: List[Int]): Int =
      foldLeft(as, 0, _ + _)

    // 3.12
    def reverse[A](as: List[A]): List[A] =
    // my solution
    //foldLeft(as, List[A](), (x, y) => Cons(y, x))
    // red book:
      foldLeft(as, Nil: List[A], (acc, a) => Cons(a, acc))

    // 3.14
    def append[A](a1: List[A], a2: List[A]): List[A] = {
      foldRight(a1, a2, (acc, a) => Cons(acc, a))
    }

    // 3.16
    def addOne(as: List[Int]) = {
      foldRight(as, Nil: List[Int], (a, acc) => Cons(a + 1, acc))
    }

    // 3.18
    def map[A, B](as: List[A], f: A => B): List[B] = {
      foldRight(as, Nil: List[B], (a, acc) => Cons(f(a), acc))
    }

    // 3.20
    def flatMap[A, B](as: List[A], f: A => List[B]): List[B] = {
      foldRight(as, Nil: List[B], (a, acc) => append(f(a), acc))
    }

    // 3.21
    def filterWithFlatMap[A](as: List[A], f: A => Boolean): List[A] = {
      def helperFun(v: A): List[A] = {
        if f(v) then List(v)
        else Nil: List[A]
      }
      flatMap(as, helperFun)
    }

    // 3.22
    def addPairwise(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
      case (Nil, Nil) => Nil
      case (Cons(h1,t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
    }

  List.apply(1, 2, 3)

  val ex1 = List.Nil
  // note the difference:
  val ex2: List[Double] = List.Nil
  val ex3: List[Int] = List.Cons(1, List.Nil)
  val ex4: List[Int] = List.Cons(1, List.Cons(2, List.Nil))
  val ex5: List[Int] = List.Cons(122, ex4)

  List.tail(ex4)
  List.setHead(108, ex4)
  List.drop(ex4, 1)
  List.drop(ex4, 2)
  List.drop(ex4, 3)
  List.drop(ex4, 4)

  List.init(ex4)
  List.init(ex5)

  // those are identities
  List.foldRight(ex4, List.Nil: List[Int], List.Cons(_, _))
  List.length(ex4)
  // 3.9
  List.length(ex3)

  //3.12
  List.reverse(List(108, 110, 112))
  List.append(List(1, 2, 3), List(107, 108, 110))

  //3.14
  List.addOne(List(1, 2, 3))

  // 3.20
  List.flatMap(List(1, 2, 3), i => List(i, i))

  // 3.21
  List.filterWithFlatMap(List(1, 2, 3, 108, 220, 22), _ <= 108)

  // 3.22
  List.addPairwise(List(1, 2, 3), List(9, 7, 5))

}