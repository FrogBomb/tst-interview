package problems

import cats.effect.*
import RateCode.*
import RateGroup.*
import CabinCode.*
import StrEnumConvertions.given

object BestPriceForGroupFinder extends IOApp.Simple {
  def getBestGroupPrices(
      rates: Seq[Rate],
      prices: Seq[CabinPrice]
  ): Seq[BestGroupPrice] =
    val rate_code_to_group_map =
      rates.groupMapReduce(rate => rate.rateCode)(rate => rate.rateGroup)(
        (groupa, groupb) =>
          if (groupa != groupb) {
            // Could have a better validation error here, or just not fail and always take the first/last one
            throw RuntimeException(
              f"A rate code matches more than one group: $groupa, $groupb"
            )
          } else groupb
      )
    val results_map: Map[CabinCodeAndGroup, RateCodeAndPrice] =
      prices.groupMapReduce(price =>
        CabinCodeAndGroup(
          cabinCode = price.cabinCode,
          rateGroup = rate_code_to_group_map(price.rateCode)
        )
      )(price =>
        RateCodeAndPrice(rateCode = price.rateCode, price = price.price)
      )((pricea, priceb) => pricea.min(priceb))
    results_map
      .map((codeAndGroup, rateAndPrice) =>
        BestGroupPrice(
          cabinCode = codeAndGroup.cabinCode,
          rateCode = rateAndPrice.rateCode,
          price = rateAndPrice.price,
          rateGroup = codeAndGroup.rateGroup
        )
      )
      .toList

  def runExample: Seq[BestGroupPrice] =
    val rates = List(
      Rate(M1, Military),
      Rate(M2, Military),
      Rate(S1, Senior),
      Rate(S2, Senior)
    )
    val prices = List(
      CabinPrice(CA, M1, 200.00),
      CabinPrice(CA, M2, 250.00),
      CabinPrice(CA, S1, 225.00),
      CabinPrice(CA, S2, 260.00),
      CabinPrice(CB, M1, 230.00),
      CabinPrice(CB, M2, 260.00),
      CabinPrice(CB, S1, 245.00),
      CabinPrice(CB, S2, 270.00)
    )

    getBestGroupPrices(rates, prices)

  def run: IO[Unit] = for {
    res <- IO(runExample)
    _ <- IO.println(res.mkString("\n"))
  } yield ExitCode.Success

}

case class Rate(rateCode: String, rateGroup: String)
case class CabinPrice(
    cabinCode: String,
    rateCode: String,
    price: BigDecimal
)
case class BestGroupPrice(
    cabinCode: String,
    rateCode: String,
    price: BigDecimal,
    rateGroup: String
)

case class CabinCodeAndGroup(cabinCode: String, rateGroup: String)
case class RateCodeAndPrice(rateCode: String, price: BigDecimal) {
  def min(that: RateCodeAndPrice): RateCodeAndPrice =
    if (price > that.price) that else this
}

// Fancy enumeration for "future" compile-time validation, extended logic on known rate groups and codes, and potentially better type safety of the existing case classes
// ... I also might just be playing with scala3 features here instead of just interpreting the instructions as string inputs.
enum RateCode(val value: String) {
  case M1 extends RateCode("M1")
  case M2 extends RateCode("M2")
  case S1 extends RateCode("S1")
  case S2 extends RateCode("S2")
  case Other(override val value: String) extends RateCode(value)
}

enum RateGroup(val value: String) {
  case Military extends RateGroup("Military")
  case Senior extends RateGroup("Senior")
  case Other(override val value: String) extends RateGroup(value)
}

enum CabinCode(val value: String) {
  case CA extends CabinCode("CA")
  case CB extends CabinCode("CB")
  case Other(override val value: String) extends CabinCode(value)
}

object StrEnumConvertions {
  given fromRateCode2str: Conversion[RateCode, String] = (rc: RateCode) =>
    rc.value
  given fromRateGroup2str: Conversion[RateGroup, String] = (rg: RateGroup) =>
    rg.value
  given fromCabinCode2str: Conversion[CabinCode, String] = (cc: CabinCode) =>
    cc.value
}
