package io.ryuichi

import org.scalatest._

class FamilySpec extends WordSpec with Matchers {

  "Family" should {
    import helper.FamilyData._

    val familyQuery = new FamilyQuery(family)

    "find a father" in {
      familyQuery.ask(Father, Ish) shouldBe List(Shan)
      familyQuery.ask(Father, Lavnya) shouldBe List(Vila)
    }

    "find a mother" in {
      familyQuery.ask(Mother, Misa) shouldBe List(Mina)
    }

    "find children" in {
      familyQuery.ask(Children, Vich) shouldBe List(Vila, Chika)
      familyQuery.ask(Children, Gru) shouldBe List()
    }

    "find sons" in {
      familyQuery.ask(Son, Vich) shouldBe List(Vila)
    }

    "find brothers" in {
      familyQuery.ask(Brother, Ish) shouldBe List(Chit, Vich)
    }

    "find sisters" in {
      familyQuery.ask(Sister, Ish) shouldBe List(Satya)
    }

    "find grand daughters" in {
      familyQuery.ask(GrandDaughter, Shan) shouldBe List(Chika, Satvy)
    }

    "find cousins" in {
      familyQuery.ask(Cousin, Vila) shouldBe List(Drita, Vrita, Satvy, Savya, Saayan)
    }

    "find brothers-in-law" in {
      familyQuery.ask(BrotherInLaw, Ish) shouldBe List(Vyan)
      familyQuery.ask(BrotherInLaw, Ambi) shouldBe List(Ish, Vich)
    }

    "find sisters-in-law" in {
      familyQuery.ask(SisterInLaw, Satya) shouldBe List(Ambi, Lika)
      familyQuery.ask(SisterInLaw, Lika) shouldBe List(Satya)
    }

    "find maternal aunts" in {
      familyQuery.ask(MaternalAunt, Satvy) shouldBe List(Ambi, Lika)
    }

    "find paternal aunts" in {
      familyQuery.ask(PaternalAunt, Vila) shouldBe List(Satya, Ambi)
    }

    "find maternal uncles" in {
      familyQuery.ask(MaternalUncle, Satvy) shouldBe List(Ish, Chit, Vich)
    }

    "find paternal uncles" in {
      familyQuery.ask(PaternalUncle, Kriya) shouldBe List(Saayan, Asva)
    }

    "grow" in {
      val Vanya = Person("Vanya", F)
      val familyQuery = new FamilyQuery(family.add(Relationship(Parent, Lavnya, Vanya)))

      familyQuery.ask(GrandChildren, Jnki) shouldBe List(Vanya)
    }

    "find mothers who have the most daughters" in {
      familyQuery.hasMostDaughter shouldBe List(Jaya, Lika, Jnki, Satya)
    }

    "describe the relationship between 2 people" in {
      familyQuery.tellRelation(Anga, Shan) shouldBe Some(Husband)
      familyQuery.tellRelation(Kriya, Saayan) shouldBe Some(PaternalUncle)
    }
  }
}
