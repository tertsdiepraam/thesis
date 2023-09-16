// Users that are admins
val admins = [0,1,2]

fun is_admin(user_id: int): bool
  admins.any(fn(x) x == user_id)

fun all_admins(l: list<int>): bool
  l.map(is_admin).foldl(True, (&&))

val result = all_admins([0,1,2,3])

fun main()
  println(result)
