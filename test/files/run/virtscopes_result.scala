object Test extends App {
  trait OptiML
  trait OptiMLExp { def apply(): Any; println(apply()) }
  def OptiML[R](b: => R) = new Scope[OptiML, OptiMLExp, R](b)

  val block = OptiML {
    object meh
    val f = (x: Int) => "foo "+ x
    f(10)
  }
  val x: String = block()
  println("x out of block: " + x)
}
