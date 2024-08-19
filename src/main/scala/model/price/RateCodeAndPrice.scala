package model.price

case class RateCodeAndPrice(rateCode: String, price: BigDecimal) {
  def min(that: RateCodeAndPrice): RateCodeAndPrice =
    if (price > that.price) that else this
}
