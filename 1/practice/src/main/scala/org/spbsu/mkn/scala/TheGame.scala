package org.spbsu.mkn.scala

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.Random

object TheGame {

  sealed trait GuessResult

  case class Correct(numTries: Int) extends GuessResult

  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  class RepeatingDigitsException extends RuntimeException

  class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException

  private val symbols = ('0' to '9').mkString + ('A' to 'Z').mkString

  def generateNumberString(length: Int): String = Random.shuffle(symbols).toString take length

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    if (userInput.length != secret.length)
      throw new WrongNumberLengthException(secret.length, userInput.length)
    val set = secret.toSet
    if (set.size != secret.length)
      throw new RepeatingDigitsException
    var bulls: Int = 0
    var cows: Int = 0
    for (i <- 0 until secret.length) {
      if (userInput(i) == secret(i))
        bulls += 1
      else if (set.contains(userInput(i)))
        cows += 1
    }
    if (bulls == secret.length) Correct(numTries)
    else Incorrect(bulls, cows)
  }

  def readInt(): Int = {
    var input = readLine().toIntOption
    while (input.isEmpty) {
      print("Incorrect input, try again: ")
      input = readLine().toIntOption
    }
    val Some(ret) = input
    ret
  }

  def startGame(): String = {
    print("Enter length of secret: ")
    val len = readInt()
    val secret = generateNumberString(len)
    println("Secret generated!")
    secret
  }

  def endGame(numTries: Int): Unit = {
    println(s"Correct! Number of tries: $numTries.")
  }

  @tailrec
  def userTry(secret: String, numTries: Int = 1): GuessResult = {
    print("Enter your guess: ")
    val userInput = readLine()
    try {
      validate(secret, userInput, numTries) match {
        case Correct(x) => return Correct(x)
        case Incorrect(bulls, cows) => println(s"Bulls: $bulls, Cows: $cows")
      }
    } catch {
      case _: WrongNumberLengthException => println("Incorrect length!")
    }
    userTry(secret, numTries + 1)
  }

  def main(args: Array[String]): Unit = {
    print("Enter your name: ")
    val name = readLine()
    println(s"Hello, $name!")

    try {
      val secret = startGame()
      val Correct(numTries) = userTry(secret)
      endGame(numTries)
    } catch {
      case _: RepeatingDigitsException => println("Not allow repeating digits in secret, something gone wrong!")
      case _ => println("Something gone wrong!")
    }
  }
}
