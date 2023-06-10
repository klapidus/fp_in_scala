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

}