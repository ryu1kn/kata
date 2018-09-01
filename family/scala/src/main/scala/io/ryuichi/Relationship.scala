package io.ryuichi

case class Relationship(relationType: BaseRelation, person1: Person, person2: Person)

sealed trait BaseRelation

sealed trait DeducibleRelation {
  def of(source: Person)(implicit f: Family): List[Person]
}

object Parent extends BaseRelation with DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    f.ask { case Relationship(Parent, parent, `source`) => parent }
}

object Spouse extends BaseRelation with DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    f.ask {
      case Relationship(Spouse, partner, `source`) => partner
      case Relationship(Spouse, `source`, partner) => partner
    }
}

object Father extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    Parent.of(source).filter(_.sex == M)
}

object Mother extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    Parent.of(source).filter(_.sex == F)
}

object Children extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    f.ask { case Relationship(Parent, `source`, children) => children }
}

object GrandChildren extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    for {
      child <- Children.of(source)
      grandchild <- Children.of(child)
    } yield grandchild
}

object Son extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    Children.of(source).filter(_.sex == M)
}

object Daughter extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    Children.of(source).filter(_.sex == F)
}

object Brother extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    for {
      father <- Father.of(source)
      son <- Son.of(father)
      if son != source
    } yield son
}

object Sister extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    for {
      father <- Father.of(source)
      daughter <- Daughter.of(father)
      if daughter != source
    } yield daughter
}

object GrandDaughter extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    GrandChildren.of(source).filter(_.sex == F)
}

object Sibling extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    for {
      parent <- Father.of(source)
      child <- Children.of(parent)
      if child != source
    } yield child
}

object Cousin extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    for {
      parent <- Father.of(source)
      uncleNaunt <- Sibling.of(parent)
      cousin <- Children.of(uncleNaunt)
    } yield cousin
}

object Husband extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    Spouse.of(source).filter(_.sex == M)
}

object Wife extends DeducibleRelation {
  override def of(source: Person)(implicit f: Family): List[Person] =
    Spouse.of(source).filter(_.sex == F)
}

object BrotherInLaw extends DeducibleRelation {
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

object SisterInLaw extends DeducibleRelation {
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

object MaternalAunt extends DeducibleRelation {
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
      sisterInLaw <- SisterInLaw.of(mother)
    } yield sisterInLaw
}

object PaternalAunt extends DeducibleRelation {
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
      sisterInLaw <- SisterInLaw.of(father)
    } yield sisterInLaw
}

object MaternalUncle extends DeducibleRelation {
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
      brotherInLaw <- BrotherInLaw.of(mother)
    } yield brotherInLaw
}

object PaternalUncle extends DeducibleRelation {
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
      brotherInLaw <- BrotherInLaw.of(father)
    } yield brotherInLaw
}
