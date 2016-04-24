package cyclo

class CycloSuite extends CommonSuite {

  import Cyclos.{arbCyclo, NonZero}

  checkAll("Cyclo field", spire.laws.RingLaws[Cyclo].field)

}
