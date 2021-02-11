// todo: "try/catch"
try {
  throw new RuntimeException("321")
} catch {
  case _: IllegalArgumentException => print("hell")
  case e: RuntimeException => println(s"Error ${e.getMessage}")
}