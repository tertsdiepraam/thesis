// Users that are admins
val admins = [0, 1, 2]

alias log<a> = (v: a, msg: string)

fun pure(v: a): log<a> { (v, "") }

fun bind(m: log<a>, k: a -> log<b>): log<b>
  val (a, s) = m
  val (b, s') = k(a)
  (b, s ++ s')

fun log(msg: string): log<()>
  ((), msg)

fun all_admins(list)
  match list
    Nil() -> pure(True)
    Cons(x, xs) -> is_admin(x) @ fn(y)
      all_admins(xs) @ fn(ys)
        pure(y && ys)

fun is_admin(user_id: int): log<bool>
  if admins.any(fn(x) x == user_id)
  then pure(True)
  else
    log("Denied " ++ show(user_id) ++ "\n") @ fn(())
      pure(False)

fun main()
  val (result, log) = all_admins([1,2,3,4])
  println(result)
  println(log)
