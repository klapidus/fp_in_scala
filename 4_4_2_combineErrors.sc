{

  enum Validated[+E, +A]:
    case Valid(get: A)
    case Invalid(errors: E)

    def map2[EE >: E, B, C](b: Validated[EE, B])
                           (f: (A, B) => C)
                           (combineErrors: (EE,EE) => EE): Validated[EE, C] =
      (this, b) match {
        case (Valid(a), Valid(b)) => Valid(f(a,b))
        case (Valid(_), Invalid(es)) => Invalid(es)
        case (Invalid(es), Valid(_)) => Invalid(es)
        // this can be generalized with a Monoid[E]
        // for now explicitely pass combineErrros
        case (Invalid(es1), Invalid(es2)) =>
          Invalid(combineErrors(es1, es2))
      }

  object Validated:
    def traverse[E, A, B](as: List[A])
                         (f: A => Validated[E, B])
                         (combineErrors: (E, E) => E): Validated[E, List[B]] = {
      val z = Valid(Nil): Validated[E, List[B]]
      as.foldRight(z){
        (a, acc) => f(a).map2(acc)(_ :: _)(combineErrors)
      }
    }

}