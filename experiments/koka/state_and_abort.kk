effect state<a>
  ctl get(): a
  ctl put(x: a): ()

effect abort
  ctl abort(): a

val hAbort = handler
  return(x)   Just(x)
  ctl abort() Nothing

fun hState(init: s, c: () -> <state<s>|e> a): e (s, a)
  fun h(c')
    with handler
      return(x)  fn(s) (s, x)
      ctl get()  fn(s) resume(s)(s)
      ctl put(n) fn(_) resume(())(n)
    c'()
  
  h(c)(init)

fun foo()
  val s = get()
  if s == 0 then
    abort()
  
  put(s - 1)
  s - 1

fun printMaybe(m: maybe<(int, int)>)
  match m
    Just((x, y)) -> println(show(x) ++ " " ++ show(y))
    Nothing -> println("nothing!")

fun main()
  printMaybe(hAbort { hState(3, foo) }) // prints "2"
  printMaybe(hAbort { hState(0, foo) }) // prints "nothing!"
