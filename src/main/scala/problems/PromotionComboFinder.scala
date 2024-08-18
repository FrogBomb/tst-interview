package problems

import cats.effect.*
import scala.annotation.tailrec
import cats.instances.boolean

object PromotionComboFinder extends IOApp.Simple {

  def allCombinablePromotions(
      allPromotions: Seq[Promotion]
  ): Seq[PromotionCombo] =
    allPromotions
      .foldLeft(
        Set[Set[String]](
          // Initially, we ignore all restrictions, and then apply the restriction one promotion at a time, building new combos
          allPromotions.map(_.code).toSet
        )
      )((agg, promoToConsiderRestrictions) =>
        agg.flatMap {
          case combo if combo.contains(promoToConsiderRestrictions.code) =>
            // canCombineCodes is always going to be non-empty, since it will also contain the current code, and promotions shouldn't be incompatible with themselves
            val (cannotCombineCodes, canCombineCodes) = combo.partition(code =>
              promoToConsiderRestrictions.cannotCombineWith(code)
            )
            if (cannotCombineCodes.isEmpty) { // If this is true, we can just keep the code in the existing combo, and move to the next
              Set(combo)
            } else {
              Set(combo - promoToConsiderRestrictions.code, canCombineCodes)
            }
          case combo => Set(combo)
        }
      )
      .map(combo => PromotionCombo(combo.toSeq.sorted))
      .toSeq

  def combinablePromotions(
      promotionCode: String,
      allPromotions: Seq[Promotion]
  ): Seq[PromotionCombo] = allCombinablePromotions(
    allPromotions.filterNot(promoToFilter =>
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

case class Promotion(code: String, notCombinableWith: Seq[String]) {
  assert(!notCombinableWith.contains(code), f"Promotion must be combinable with itself: $code")
  val cannotCombineSet = notCombinableWith.toSet
  def cannotCombineWith(code: String): Boolean = cannotCombineSet.contains(code)
}

case class PromotionCombo(promotionCodes: Seq[String]) {
  def +(promotionCode: String) = PromotionCombo(promotionCodes :+ promotionCode)
}
