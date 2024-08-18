package problems

import cats.effect.*
import munit.CatsEffectSuite
import problems.PromotionComboFinder
import problems.PromotionComboFinder.allCombinablePromotions
import problems.PromotionComboFinder.combinablePromotions

class PromotionComboFinderSuite extends CatsEffectSuite:

  test(
    "example for PromotionComboFinder matches documented problem result"
  ) {
    val exampleResults = PromotionComboFinder.runExamples
    val allExample = exampleResults("allCombinablePromotions")
    val p1Example = exampleResults("combinablePromotions for P1")
    val p3Example = exampleResults("combinablePromotions for P3")

    assertEquals(
      allExample.toSet.map(_.promotionCodes.toSet),
      Set(
        PromotionCombo(Seq("P1", "P2")),
        PromotionCombo(Seq("P1", "P4", "P5")),
        PromotionCombo(Seq("P2", "P3")),
        PromotionCombo(Seq("P3", "P4", "P5"))
      ).map(_.promotionCodes.toSet)
    )
    assertEquals(
      p1Example.toSet.map(_.promotionCodes.toSet),
      Set(
        PromotionCombo(Seq("P1", "P2")),
        PromotionCombo(Seq("P1", "P4", "P5"))
      ).map(_.promotionCodes.toSet)
    )
    assertEquals(
      p3Example.toSet.map(_.promotionCodes.toSet),
      Set(
        PromotionCombo(Seq("P2", "P3")),
        PromotionCombo(Seq("P3", "P4", "P5"))
      ).map(_.promotionCodes.toSet)
    )

  }

  test("Free Combos") {
    val promos = List(
      Promotion("F1", Seq()),
      Promotion("F2", Seq()),
      Promotion("F3", Seq()),
      Promotion("F4", Seq()),
      Promotion("F5", Seq()),
      Promotion("F6", Seq())
    )
    val allCombos = allCombinablePromotions(promos)
    assertEquals(
      allCombos.toSet.map(_.promotionCodes.toSet),
      Set(promos.toSet.map(_.code))
    )
    assertEquals(
      combinablePromotions("F1", promos).toSet.map((_.promotionCodes.toSet)),
      Set(promos.toSet.map(_.code))
    )
  }

  test("Can only be one combos") {
    val promos = List(
      Promotion("H1", Seq("H2", "H3", "H4", "H5", "H6")),
      Promotion("H2", Seq("H1", "H3", "H4", "H5", "H6")),
      Promotion("H3", Seq("H1", "H2", "H4", "H5", "H6")),
      Promotion("H4", Seq("H1", "H2", "H3", "H5", "H6")),
      Promotion("H5", Seq("H1", "H2", "H3", "H4", "H6")),
      Promotion("H6", Seq("H1", "H2", "H3", "H4", "H5"))
    )

    val allCombos = allCombinablePromotions(promos)
    assertEquals(
      allCombos.toSet.map(_.promotionCodes.toSet),
      promos.map(v => Set(v.code)).toSet
    )
    for(
      promo <- promos
    ){
      val promoCombos = combinablePromotions(promo.code, promos)
      assertEquals(
        Set(Set(promo.code)),
        promoCombos.toSet.map(_.promotionCodes.toSet)
      )
    }
  }

  test(
    "A made up, more complex example to test"
  ) {
    val promos = List(
      Promotion("P1", Seq("P3", "P4")), // P1 is not combinable with P3
      Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
      Promotion("P3", Seq("P1", "P7")), // P3 is not combinable with P1 and P7
      Promotion("P4", Seq("P1", "P2", "P7")), // P4 is not combinable with P2
      Promotion("P5", Seq("P2")), // P5 is not combinable with P2
      Promotion("P6", Seq()), // P6 can be combined with anything
      Promotion("P7", Seq("P3", "P4"))
    )
    val allCombos = allCombinablePromotions(promos)

    assertEquals(
      allCombos.toSet.map(_.promotionCodes.toSet),
      Set(
        PromotionCombo(
          Seq(
            "P1",
            "P5",
            "P6",
            "P7"
          )
        ),
        PromotionCombo(
          Seq(
            "P2",
            "P6",
            "P7"
          )
        ),
        PromotionCombo(
          Seq(
            "P1",
            "P2",
            "P6",
            "P7"
          )
        ),
        PromotionCombo(
          Seq(
            "P3",
            "P4",
            "P5",
            "P6"
          )
        ),
        PromotionCombo(
          Seq(
            "P2",
            "P3",
            "P6"
          )
        )
      ).map(_.promotionCodes.toSet)
    )

  }

  import cats.effect.std.Dispatcher

  val dispatcher = ResourceFunFixture(Dispatcher.parallel[IO])

end PromotionComboFinderSuite
