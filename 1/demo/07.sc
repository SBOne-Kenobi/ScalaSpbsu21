// todo: pattern matching

"12" match {
  case "123" => 123
  case _ => 0
}

122 match {
  case x if x % 2 == 0 => 1
  case _ => 2
}

//todo: match everywhere
List(1, 2, 3) match {
  case x :: xs => x
  case Nil => 0
  case _ => ???
}

val (x :: xs) = List(1, 2, 3)

//todo: name shadowing
