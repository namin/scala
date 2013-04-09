object Test {
  type Rep[x] = x
  //def __new[T](args: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] = error("")
  val foo: Rep[Struct[Rep] { val x: Int; val y: String }] = new Struct[Rep] { val x: Rep[Int] = 23; val y = "y" }
}
