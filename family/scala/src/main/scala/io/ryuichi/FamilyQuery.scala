package io.ryuichi

import common.List._

case class Family(relations: List[Relationship]) {
  private val coupleRelations: List[Relationship] = relations.filter(_.relationType == Spouse)
  private val parentRelations: List[Relationship] = relations.filter(_.relationType == Parent)

  private val members: List[Person] = relations.collectPeople
  private val rootMembers: List[Person] =
    coupleRelations.filter(cr =>
      parentRelations.forall(pr =>
        pr.person2 != cr.person1 && pr.person2 != cr.person2
      )
    ).collectPeople

  val nonRootMembers: List[Person] = members diff rootMembers

  def add(relationship: Relationship): Family = Family(relationship :: relations)

  def ask(collector: PartialFunction[Relationship, Person]): List[Person] =
    relations.collect(collector)
}

class FamilyQuery(family: Family) {
  private implicit def f: Family = family

  def hasMostDaughter: List[Person] = {
    val eligibleLadies = family.nonRootMembers.filter(_.sex == F)
    eligibleLadies.maxsBy(Daughter.of(_).size)
  }

  def ask(relationship: DeducibleRelation, person: Person): List[Person] =
    relationship.of(person)
}
