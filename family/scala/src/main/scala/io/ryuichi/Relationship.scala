package io.ryuichi

case class Relationship(relationType: BaseRelation, person1: Person, person2: Person)

object Relationship {
  implicit class RelationListWrap(relations: List[Relationship]) {
    def collectPeople: List[Person] = relations
      .flatMap(rel => List(rel.person1, rel.person2))
      .distinct
  }
}

sealed trait BaseRelation

sealed trait DeducibleRelation {
  def of(source: Person)(implicit f: Family): List[Person]
}

object DeducibleRelation {
  val types: List[DeducibleRelation] = List(
    Husband, Wife,
    Father, Mother, Son, Daughter, GrandChildren, GrandDaughter,
    Brother, Sister, BrotherInLaw, SisterInLaw,
    MaternalAunt, PaternalAunt, MaternalUncle, PaternalUncle
  )
}

case object Parent extends BaseRelation with DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    f.ask { case Relationship(Parent, parent, `source`) => parent }
}

case object Spouse extends BaseRelation with DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    f.ask {
      case Relationship(Spouse, partner, `source`) => partner
      case Relationship(Spouse, `source`, partner) => partner
    }
}

case object Father extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    Parent.of(source).filter(_.sex == M)
}

case object Mother extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    Parent.of(source).filter(_.sex == F)
}

case object Children extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    f.ask { case Relationship(Parent, `source`, children) => children }
}

case object GrandChildren extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    for {
      child <- Children.of(source)
      grandchild <- Children.of(child)
    } yield grandchild
}

case object Son extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    Children.of(source).filter(_.sex == M)
}

case object Daughter extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    Children.of(source).filter(_.sex == F)
}

case object Brother extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    for {
      father <- Father.of(source)
      son <- Son.of(father)
      if son != source
    } yield son
}

case object Sister extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    for {
      father <- Father.of(source)
      daughter <- Daughter.of(father)
      if daughter != source
    } yield daughter
}

case object GrandDaughter extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    GrandChildren.of(source).filter(_.sex == F)
}

case object Sibling extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    for {
      parent <- Father.of(source)
      child <- Children.of(parent)
      if child != source
    } yield child
}

case object Cousin extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    for {
      parent <- Father.of(source)
      uncleNaunt <- Sibling.of(parent)
      cousin <- Children.of(uncleNaunt)
    } yield cousin
}

case object Husband extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    Spouse.of(source).filter(_.sex == M)
}

case object Wife extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    Spouse.of(source).filter(_.sex == F)
}

case object BrotherInLaw extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    brothersOfHusband(source) ++ husbandOfSiblings(source)

  def brothersOfHusband(source: Person)(implicit f: Family): List[Person] =
    for {
      partner <- Spouse.of(source)
      brotherInLaw <- Brother.of(partner)
    } yield brotherInLaw

  def husbandOfSiblings(source: Person)(implicit f: Family): List[Person] =
    for {
      sibling <- Sibling.of(source)
      brotherInLaw <- Husband.of(sibling)
    } yield brotherInLaw
}

case object SisterInLaw extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    sistersOfWife(source) ++ wifeOfSiblings(source)

  def sistersOfWife(source: Person)(implicit f: Family): List[Person] =
    for {
      partner <- Spouse.of(source)
      sisterInLaw <- Sister.of(partner)
    } yield sisterInLaw

  def wifeOfSiblings(source: Person)(implicit f: Family): List[Person] =
    for {
      sibling <- Sibling.of(source)
      sisterInLaw <- Wife.of(sibling)
    } yield sisterInLaw
}

case object MaternalAunt extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    sistersOfMother(source) ++ sistersInLawOfMother(source)

  def sistersOfMother(source: Person)(implicit f: Family): List[Person] =
    for {
      mother <- Mother.of(source)
      sister <- Sister.of(mother)
    } yield sister

  def sistersInLawOfMother(source: Person)(implicit f: Family): List[Person] =
    for {
      mother <- Mother.of(source)
      brother <- Brother.of(mother)
      wife <- Wife.of(brother)
    } yield wife
}

case object PaternalAunt extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    sistersOfFather(source) ++ sistersInLawOfFather(source)

  def sistersOfFather(source: Person)(implicit f: Family): List[Person] =
    for {
      father <- Father.of(source)
      sister <- Sister.of(father)
    } yield sister

  def sistersInLawOfFather(source: Person)(implicit f: Family): List[Person] =
    for {
      father <- Father.of(source)
      brother <- Brother.of(father)
      wife <- Wife.of(brother)
    } yield wife
}

case object MaternalUncle extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    brothersOfMother(source) ++ brothersInLawOfMother(source)

  def brothersOfMother(source: Person)(implicit f: Family): List[Person] =
    for {
      mother <- Mother.of(source)
      brother <- Brother.of(mother)
    } yield brother

  def brothersInLawOfMother(source: Person)(implicit f: Family): List[Person] =
    for {
      mother <- Mother.of(source)
      sister <- Sister.of(mother)
      husband <- Husband.of(sister)
    } yield husband
}

case object PaternalUncle extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    brothersOfFather(source) ++ brothersInLawOfFather(source)

  def brothersOfFather(source: Person)(implicit f: Family): List[Person] =
    for {
      father <- Father.of(source)
      brother <- Brother.of(father)
    } yield brother

  def brothersInLawOfFather(source: Person)(implicit f: Family): List[Person] =
    for {
      father <- Father.of(source)
      sister <- Sister.of(father)
      husband <- Husband.of(sister)
    } yield husband
}
