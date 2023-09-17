type never

effect ret<a>
  ctl ret(v: a): never

fun diverge(): <div> never
  diverge()

fun add(a: int, b: int): <ret<int>> never
  ret(a + b)

fun main'(): <div,console,ret<()>> never
  with ctl ret(x: int)
    println(x)
    ret(())
  add(40, 2)

fun main(): <div,console> ()
  with ctl ret(x: ()) ()
  main'()
  ()
