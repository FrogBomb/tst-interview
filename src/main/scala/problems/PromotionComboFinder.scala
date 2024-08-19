package problems

import cats.effect.*
import scala.annotation.tailrec
import cats.instances.boolean
import model.promotions.*

object PromotionComboFinder extends IOApp.Simple {

  /*
    Find all combinable promotions
   */
  def allCombinablePromotions(
      allPromotions: Seq[Promotion]
  ): Seq[PromotionCombo] =
    allPromotions
      .foldLeft(
        Set[Set[String]](
          // Initially, we ignore all restrictions, and then apply the restriction one promotion at a time, building new combos
          allPromotions.map(_.code).toSet
        )
      )((partiallyRestrictedCombos, promoToConsiderRestrictions) => {
        val combosWithNewRestrictions = partiallyRestrictedCombos.flatMap {
          // Here, for each of the existing combos, we see if the combo contains the next code.
          case combo if combo.contains(promoToConsiderRestrictions.code) =>
            // canCombineCodes is always going to be non-empty, since it will also contain the current code, and promotions shouldn't be incompatible with themselves
            val (cannotCombineCodes, canCombineCodes) = combo.partition(code =>
              promoToConsiderRestrictions.cannotCombineWith(code)
            )
            if (cannotCombineCodes.isEmpty) {
              // If this is true, we can just keep the code in the existing combo, and move to the next
              Set(combo)
            } else {
              // Otherwise, we can produce two new sets: one with the restictions, and one without this code.
              // Note: There is a chance a combo without this code is a subset of another combo we produce at this iteration.
              //   This will be handled in a filter after we are done.
              Set(combo - promoToConsiderRestrictions.code, canCombineCodes)
            }
          // The combo does not contain the current promo we are considering restrictions for, so we don't need to change anything.
          case combo => Set(combo)
        }
        // We may produce a group that is a strict subset of another group. Remove those.
        combosWithNewRestrictions.filter(comboGroup =>
          combosWithNewRestrictions.forall(otGroup =>
            otGroup.equals(comboGroup) | (!comboGroup.subsetOf(otGroup))
          )
        )
      })
      .map(combo => PromotionCombo(combo.toSeq.sorted))
      .toSeq

  /*
    Find all promotion combos that can be combined with a given code
   */
  def combinablePromotions(
      promotionCode: String,
      allPromotions: Seq[Promotion]
  ): Seq[PromotionCombo] = allCombinablePromotions(
    allPromotions.filterNot(promoToFilter =>
      // We are pre-filtering promos we know cannot be combined with the given code, then just finding all compatible combinations otherwise.
      promoToFilter.notCombinableWith.contains(promotionCode)
    )
  )

  def runExamples =
    val promos = List(
      Promotion("P1", Seq("P3")), // P1 is not combinable with P3
      Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
      Promotion("P3", Seq("P1")), // P3 is not combinable with P1
      Promotion("P4", Seq("P2")), // P4 is not combinable with P2
      Promotion("P5", Seq("P2")) // P5 is not combinable with P
    )
    Map(
      "allCombinablePromotions" -> allCombinablePromotions(promos),
      "combinablePromotions for P1" -> combinablePromotions("P1", promos),
      "combinablePromotions for P3" -> combinablePromotions("P3", promos)
    )

  def run: IO[Unit] = for {
    res <- IO(runExamples)
    _ <- res.toList.foldLeft(IO.unit) { case (prevIO, (name, nextRes)) =>
      for {
        _ <- prevIO
        _ <- IO.println("")
        _ <- IO.println(name)
        _ <- IO.println(f"  ${nextRes.mkString("\n  ")}")
      } yield { IO.unit }
    }
  } yield ExitCode.Success
}
