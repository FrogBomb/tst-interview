package model.promotions

case class PromotionCombo(promotionCodes: Seq[String]) {
  def +(promotionCode: String) = PromotionCombo(promotionCodes :+ promotionCode)
}
