fun should_be_pure(x: int): int
  println("This should give a type error!")
  x + 10

fun main()
  should_be_pure(10)