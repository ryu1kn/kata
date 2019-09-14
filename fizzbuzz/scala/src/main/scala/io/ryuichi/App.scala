package io.ryuichi

object App {
  def fizzbuzz: Int => String = {
    case n if n % 15 == 0 => "FizzBuzz"
    case n if n % 5 == 0 => "Buzz"
    case n if n % 3 == 0 => "Fizz"
    case n => n.toString
  }
}
