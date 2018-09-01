package io.ryuichi

sealed trait Sex
object M extends Sex
object F extends Sex

case class Person(name: String, sex: Sex) {
  override def toString: String = name
}
