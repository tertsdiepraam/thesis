// Users that are admins
val admins = [0,1,2]

effect log
  ctl log(msg: string): ()

val hLog = handler
  return(x) (x, "")
  ctl log(msg)
    val (x, msg') = resume(())
    (x, msg ++ msg')

fun is_admin(user_id: int): <log> bool
  val result = admins.any(fn(x) x == user_id)
  if !result then
    log("Denied " ++ show(user_id) ++ "\n")
  result

fun all_admins(l)
  l.map(is_admin).foldl(True, (&&))

fun main()
  val (result, log) = hLog { [1,2,3,4].all(is_admin) }
  println(result)
  println(log)
