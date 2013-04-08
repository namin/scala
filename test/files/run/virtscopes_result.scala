object Test extends App {
  trait OptiML[R]
  trait OptiMLExp[R] {
    def apply(): R
    def result: R = {
      val r = apply()
      println(r)
      r
    }
  }
  def OptiML[R](b: => R) = new Scope[OptiML[R], OptiMLExp[R], R](b)

  val x: String = OptiML[String] {
    object meh
    val f = (x: Int) => "foo "+ x
    f(10)
  }
  println("x out of block: " + x)
}
