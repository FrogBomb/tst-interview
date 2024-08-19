package problems

import cats.effect.*
import model.price.*
import munit.FunSuite
import problems.*
import problems.BestPriceForGroupFinder.getBestGroupPrices

class BestPriceGroupFinderSuite extends FunSuite:

  test(
    "example for BestPriceForGroupFinder matches documented problem result"
  ) {

    assertEquals(
      BestPriceForGroupFinder.runExample.toSet,
      Set(
        BestGroupPrice("CA", "M1", 200.00, "Military"),
        BestGroupPrice("CA", "S1", 225.00, "Senior"),
        BestGroupPrice("CB", "M1", 230.00, "Military"),
        BestGroupPrice("CB", "S1", 245.00, "Senior")
      )
    )

  }

  test(
    "More complex case"
  ) {
    val rates = List(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("M3", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior"),
      Rate("S3", "Senior"),
      Rate("P1", "Premium User"),
      Rate("P2", "Premium User"),
      Rate("P3", "Premium User")
    )
    val prices = List(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "M3", 100.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CA", "S3", 150.00),
      CabinPrice("CA", "P1", 125.00),
      CabinPrice("CA", "P2", 260.00),
      CabinPrice("CA", "P3", 150.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "M3", 270.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 170.00),
      CabinPrice("CB", "S3", 270.00),
      CabinPrice("CB", "P1", 170.45),
      CabinPrice("CB", "P2", 170.50),
      CabinPrice("CB", "P3", 170.60)
    )

    val res = getBestGroupPrices(rates, prices)
    assertEquals(
      res.toSet,
      Set(
        BestGroupPrice("CA", "M3", 100.00, "Military"),
        BestGroupPrice("CA", "S3", 150.00, "Senior"),
        BestGroupPrice("CA", "P1", 125.00, "Premium User"),
        BestGroupPrice("CB", "M1", 230.00, "Military"),
        BestGroupPrice("CB", "S2", 170.00, "Senior"),
        BestGroupPrice("CB", "P1", 170.45, "Premium User")
      )
    )
  }

end BestPriceGroupFinderSuite
