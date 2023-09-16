fun fibonacci(a: int, b: int, n: int): <div> int
  if n <= 1 then
    a
  else
    fibonacci(b, a+b, n - 1)

effect my_state
  fun my_get() : (int, int)
  fun my_set( x : (int, int) ) : ()

fun state( init : (int, int), action : () -> <my_state|e> b ) : e b 
  var st := init
  with handler
    fun my_get()  st 
    fun my_set(i) st := i 
  action()

fun fibonacci2(n: int): <my_state,div|e> int
  if n <= 1 then
    fst(my_get())
  else
    val (a, b) = my_get()
    my_set((b, a + b))
    fibonacci2(n - 1)


fn check_balanced(x: str):
  match head(x) {
    
  }

fun main()
  println(fibonacci(0, 1, 5))
  println(state((0, 1), fn() { fibonacci2(5) }))
