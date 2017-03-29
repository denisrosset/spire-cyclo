package cyclo

class CycloSuite extends CommonSuite {

  import Cyclos.{arbCyclo, arbRealCyclo, NonZero, RealNonZero}

  checkAll("Cyclo field", spire.laws.RingLaws[Cyclo].field)

  checkAll("Real cyclo field", spire.laws.RingLaws[RealCyclo].field)

  checkAll("Real cyclo signed", spire.laws.OrderLaws[RealCyclo].order) //TODO truncatedDivision

  checkAll("Real cyclo signed additive", spire.laws.CombinationLaws[RealCyclo].signedAdditiveAbGroup)

}
