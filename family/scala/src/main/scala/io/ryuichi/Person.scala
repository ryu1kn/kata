package io.ryuichi

sealed trait Sex
case object M extends Sex
case object F extends Sex

case class Person(name: String, sex: Sex) {
  override def toString: String = name
}
