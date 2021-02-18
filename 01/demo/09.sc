"Anonymous functions"

//Functional type/
val inc: Int => Int = i => i * i

//With explicit parameter
val inc = (i : Int) => i * i

//Placeholder syntax
val inc: Int => Int = _ + 1

//partially applied function syntax
def f(a: Int, b: Int): Int = a * b

val x: Int => Int = f(4, _)

x(3)