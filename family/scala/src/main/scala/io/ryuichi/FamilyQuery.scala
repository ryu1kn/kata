package io.ryuichi

case class Family(relations: List[Relationship]) {
  def add(relationship: Relationship): Family = Family(relationship :: relations)

  def ask(collector: PartialFunction[Relationship, Person]): List[Person] =
    relations.collect(collector)
}

class FamilyQuery(family: Family) {

  def ask(relationship: DeducibleRelation, person: Person): List[Person] =
    relationship.of(person)(family)

}
