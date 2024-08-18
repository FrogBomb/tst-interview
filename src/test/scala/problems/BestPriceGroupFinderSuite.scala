package problems

import cats.effect.*
import munit.CatsEffectSuite
import problems.*
import RateCode.*
import RateGroup.*
import CabinCode.*
import StrEnumConvertions.given

class BestPriceGroupFinderSuite extends CatsEffectSuite:

  test(
    "example for BestPriceForGroupFinder matches documented problem result"
  ) {

    assertEquals(
      BestPriceForGroupFinder.runExample.toSet,
      Set(
        BestGroupPrice(CA, M1, 200.00, Military),
        BestGroupPrice(CA, S1, 225.00, Senior),
        BestGroupPrice(CB, M1, 230.00, Military),
        BestGroupPrice(CB, S1, 245.00, Senior)
      )
    )

  }

end BestPriceGroupFinderSuite
