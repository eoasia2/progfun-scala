package calculator


object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var((b() * b()) - 4 * c() * a())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(
      if (delta() < 0) Set.empty
      else Set(
        (-b() + math.sqrt(delta())) / (2 * a()), (-b() - math.sqrt(delta())) / (2 * a()))
    )
  }
}
//
//Signal(delta() match {
//case n if n < 0 => Set.empty
//case _ =>
//Set(
//(-b() + math.sqrt(delta())) / (2 * a()),
//(-b() - math.sqrt(delta())) / (2 * a())
//)
//})