package problems

import cats.effect.*
import munit.CatsEffectSuite
import problems.PromotionComboFinder

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

end PromotionComboFinderSuite
