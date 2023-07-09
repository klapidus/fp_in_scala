case class State[S, +A](run: S => (A, S)):
  def map[B](f: A => B): State[S, B] =
    State[S, B]{
      s => {
        val (a, state_next) = this.run(s)
        (f(a), state_next)
      }
    }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State[S, B] {
      s => {
        val (a, state_next) = this.run(s)
        f(a).run(state_next)
      }
    }

  def mapViaFlatMap[B](f: A => B): State[S, B] =
    // inside we need a: A => State[S, A]
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    State[S, C] {
      s => {
        val (a, s1) = this.run(s)
        val (b, s2) = sb.run(s1)
        (f(a, b), s2)
      }
    }
  }

object State:
  def unit[S, A](value: A): State[S, A] = {
    new State[S, A](run = s => (value, s))
  }