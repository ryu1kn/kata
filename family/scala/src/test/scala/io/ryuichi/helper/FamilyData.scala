package io.ryuichi.helper

import io.ryuichi._

object FamilyData {
  val Shan = Person("Shan", M)
  val Ish = Person("Ish", M)
  val Chit = Person("Chit", M)
  val Drita = Person("Drita", M)
  val Vrita = Person("Vrita", M)
  val Jata = Person("Jata", M)
  val Mnu = Person("Mnu", M)
  val Vich = Person("Vich", M)
  val Vila = Person("Vila", M)
  val Kpila = Person("Kpila", M)
  val Gru = Person("Gru", M)
  val Vyan = Person("Vyan", M)
  val Asva = Person("Asva", M)
  val Savya = Person("Savya", M)
  val Kriya = Person("Kriya", M)
  val Saayan = Person("Saayan", M)
  val Misa = Person("Misa", M)

  val Anga = Person("Anga", F)
  val Ambi = Person("Ambi", F)
  val Jaya = Person("Jaya", F)
  val Driya = Person("Driya", F)
  val Lika = Person("Lika", F)
  val Jnki = Person("Jnki", F)
  val Lavnya = Person("Lavnya", F)
  val Chika = Person("Chika", F)
  val Satya = Person("Satya", F)
  val Satvy = Person("Satvy", F)
  val Krpi = Person("Krpi", F)
  val Mina = Person("Mina", F)

  val family = Family(List(
    Relationship(Spouse, Shan, Anga),
    Relationship(Spouse, Chit, Ambi),
    Relationship(Spouse, Drita, Jaya),
    Relationship(Spouse, Driya, Mnu),
    Relationship(Spouse, Vich, Lika),
    Relationship(Spouse, Vila, Jnki),
    Relationship(Spouse, Chika, Kpila),
    Relationship(Spouse, Lavnya, Gru),
    Relationship(Spouse, Satya, Vyan),
    Relationship(Spouse, Satvy, Asva),
    Relationship(Spouse, Savya, Krpi),
    Relationship(Spouse, Saayan, Mina),

    Relationship(Parent, Shan, Ish),
    Relationship(Parent, Shan, Chit),
    Relationship(Parent, Chit, Drita),
    Relationship(Parent, Drita, Jata),
    Relationship(Parent, Drita, Driya),
    Relationship(Parent, Chit, Vrita),
    Relationship(Parent, Shan, Vich),
    Relationship(Parent, Vich, Vila),
    Relationship(Parent, Vila, Lavnya),
    Relationship(Parent, Vich, Chika),
    Relationship(Parent, Shan, Satya),
    Relationship(Parent, Vyan, Satvy),
    Relationship(Parent, Vyan, Savya),
    Relationship(Parent, Savya, Kriya),
    Relationship(Parent, Vyan, Saayan),
    Relationship(Parent, Saayan, Misa),
    Relationship(Parent, Anga, Ish),
    Relationship(Parent, Anga, Chit),
    Relationship(Parent, Anga, Vich),
    Relationship(Parent, Anga, Satya),
    Relationship(Parent, Ambi, Drita),
    Relationship(Parent, Ambi, Vrita),
    Relationship(Parent, Jaya, Jata),
    Relationship(Parent, Jaya, Driya),
    Relationship(Parent, Lika, Vila),
    Relationship(Parent, Lika, Chika),
    Relationship(Parent, Jnki, Lavnya),
    Relationship(Parent, Satya, Satvy),
    Relationship(Parent, Satya, Savya),
    Relationship(Parent, Satya, Saayan),
    Relationship(Parent, Krpi, Kriya),
    Relationship(Parent, Mina, Misa)
  ))
}
