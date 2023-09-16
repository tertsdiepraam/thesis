// Users that are admins
val admins = [0,1,2]

fun all_admins(list: list<int>): (bool, string)
  match list
    Nil() -> (True, "")
    Cons(x, xs) ->
      val (y, s) = is_admin(x)
      val (ys, s') = all_admins(xs)
      (y && ys, s ++ s')

fun is_admin(user_id: int): (bool, string)
  if admins.any(fn(x) x == user_id)
  then (True, "")
  else (False, "Denied " ++ show(user_id) ++ "\n")

fun main()
  val (result, log) = all_admins([1,2,3,4])
  println(result)
  println(log)
