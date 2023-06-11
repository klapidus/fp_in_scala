{
  import math.max

  enum Tree[+A]:
    case Leaf(v: A)
    case Branch(l: Tree[A], r: Tree[A])

    def depth: Int = this match {
      case Leaf(v) => 1
      // redbook solution: case Leaf(_) => 0
      //case Branch(l, r) => max(1 + depth(l), 1 + depth(r))
      // redbook solution:
      case Branch(l, r) => 1 + (l.depth.max(r.depth))
    }

  object Tree:
    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(v) => v
      case Branch(l, r) => max(maximum(l), maximum(r))
    }
    def map[A, B](t: Tree[A], f: A => B): Tree[B] = t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l, f), map(r, f))
    }
    // this one is interesting, compare with List.foldLeft
    def fold[A, B](t: Tree[A], f: A => B, g: (B, B) => B): B = t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(fold(l,f,g), fold(r,f,g))
    }
}

3.max(4)
