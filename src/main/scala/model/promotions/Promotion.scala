package model.promotions

case class Promotion(code: String, notCombinableWith: Seq[String]) {
  assert(
    !notCombinableWith.contains(code),
    f"Promotion must be combinable with itself: $code"
  )
  val cannotCombineSet = notCombinableWith.toSet
  def cannotCombineWith(code: String): Boolean = cannotCombineSet.contains(code)
}
