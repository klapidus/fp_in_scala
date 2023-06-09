def isSorted[A](as: Seq[A], gt: (A, A) => Boolean): Boolean = {
  if (as.length <= 1) true
  else {
    if (gt(as(1), as(0))) isSorted(as.tail, gt)
    else false
  }
}

isSorted(Array(1, 2, 3), _ > _)
isSorted(Array(1, 2, 1), _ > _)
isSorted(Array(3, 2, 1), _ < _)

def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
  (b: B) => f(a, b)

def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
  (a: A) => {
    (b: B) => f(a, b)
  }
}

def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {
  (a, b) => f(a)(b)
}

def compose[A, B, C](f: B => C, g: A => B): A => C = {
  a => f(g(a))
}