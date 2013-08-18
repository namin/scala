case class Complex(re: Double, im: Double) extends Struct
object Test extends EmbeddedControls {
  case class Rep[T:Manifest](x: T) {
    def selectDynamic[T](n: String): Rep[T] = error(n)
  }
  def unit[T:Manifest](x: T) = Rep(x)
  def __new[T](args: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] = error("")

  val c: Rep[Complex] = Complex(unit(1.0), unit(2.0))
  val x: Rep[Double] = c.re
}
