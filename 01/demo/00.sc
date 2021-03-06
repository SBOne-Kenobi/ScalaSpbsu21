//todo: What is Scala?

//define method isPrime
def isPrime(n: Int): Boolean = n > 1 && (2 until n).forall(n % _ != 0)

//print all primes from 1 to 100
for {
  i <- 1 to 100
  if isPrime(i)
} print(s"$i ")
