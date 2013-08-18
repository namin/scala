object Test extends EmbeddedControls {
  case class Rep[T:Manifest](x: T)
  def unit[T:Manifest](x: T) = Rep(x)
  def __new[T](args: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] = error("")
  case class Complex(re: Double, im: Double) extends Struct

  val c = Complex(unit(1.0), unit(2.0))
}
