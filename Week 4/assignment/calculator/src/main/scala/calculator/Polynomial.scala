package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {

    Signal {
      val A = a()
      val B = b()
      val C = c()

      B * B - 4 * A * C
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val A = a()
      val B = b()
      val deltaNon = delta()

      deltaNon match {
        case x if x >= 0 =>
          Set(
            (-1) * B + math.sqrt(deltaNon) / 2 * A,
            (-1) * B - math.sqrt(deltaNon) / 2 * A
          )
        case x if x < 0 => Set(0)
      }
    }
  }
}
