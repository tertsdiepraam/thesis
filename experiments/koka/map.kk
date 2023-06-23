effect yield<a>
  ctl yield(x: a): ()

val hYield = handler
  ctl yield(x: int)
    println(x)
    resume(())

fun map(f, c)
  with handler
    ctl yield(x)
      yield(f(x))
      resume(())
  c()

fun main()
  with hYield
  map(
    fn(x) 2 * x,
    {
      yield(1)
      yield(2)
      yield(3)
    }
  )
