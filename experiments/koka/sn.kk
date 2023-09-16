effect state
    ctl get(): int
    ctl put(x: int): ()

effect nondet
    ctl or(): bool

fun state(c)
    with handler
        return(x)  fn(s) x
        ctl get()  fn(s) resume(s)(s)
        ctl put(n) fn(s) resume(())(n)
    c()

fun nondet(c)
    with handler
        return(x) [x]
        ctl or()
            val a = resume(False)
            val b = resume(True)
            a ++ b
    c()

fun foo()
    or()
    put(get() + 1)
    get()

fun main()
    val xs = nondet(fn() state(foo)(0))
    val ys = state(fn() nondet(foo))(0)
    println(show(xs))
    println(show(ys))
