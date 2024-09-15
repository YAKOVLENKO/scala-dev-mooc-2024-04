package object monad {

  /**
   * Реализуйте методы map / flatMap / withFilter чтобы работал код и законы монад соблюдались
   * HINT: для проверки на пустой элемент можно использовать eq
   */

  trait Wrap[+A] {

    def get: A

    // todo А это точно должно быть в trait, а не в object?
    def pure[R](x: R): Wrap[R] = NonEmptyWrap(x)

    def flatMap[R](f: A => Wrap[R]): Wrap[R] = {
      f(get)
    }

    // HINT: map можно реализовать через pure и flatMap
    def map[R](f: A => R): Wrap[R] = {
      flatMap(f.andThen(pure))
    }

    def withFilter(f: A => Boolean): Wrap[A] = {
      if (f(get)) pure(get)
      else EmptyWrap
    }

  }

  object Wrap {
    def empty[R]: Wrap[R] = EmptyWrap
  }

  case class NonEmptyWrap[A](result: A) extends Wrap[A] {
    override def get: A = result
  } // pure

  case object EmptyWrap extends Wrap[Nothing] {
    override def get: Nothing = throw new NoSuchElementException("Wrap.get")
  } // bottom, null element

}