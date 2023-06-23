type cont<a,b,e>
  Cont( f : a -> e b )

effect subjump<a,b,e>
  ctl sub_() : either<cont<a,b,e>,a>
  ctl jump( ref : cont<a,b,e> , v : a ) : c

fun sub(f : (cont<a,b,e>) -> <subjump<a,b,e>|e> b, g : a -> <subjump<a,b,e>|e> b): <subjump<a,b,e>|e> b
  match sub_()
    Left(ref) -> f(ref)
    Right(v) -> g(v)

fun hSubjump(m : () -> <subjump<a,b,e>|e> b): e b
  with handler
    ctl sub_()
      val y = fn(x) resume(Right(x))
      resume(Left(Cont(y)))
    ctl jump(ref, v)
      val Cont(ref') = ref
      ref'(v)
  m()

fun main()
  with hSubjump
  sub(
    fn(ref) {
      val a = 5
      println("before jump")
      sub(
        fn(ref2) {
          jump(ref2, 5)
        },
        fn(a) {
          println("small jump")
          ()
        }
      )
      jump(ref, a)
      println("before jump")
      ()
    },
    fn(a) {
      println("jumped!")
      sub(
        fn(ref2) {
          jump(ref2, 5)
        },
        fn(a) {
          println("small jump 2")
          ()
        }
      )
      ()
    }
  )

  println("end")