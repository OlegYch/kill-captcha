class TrainNetworkTest extends org.scalatest.FunSuite {
  test("should produce b/w values") {
    new TrainNetwork {
      val input = readImageInput(ImageUtils.readImage(getClass.getResourceAsStream("8.png")))
      println(input.map(BigDecimal(_).setScale(4, BigDecimal.RoundingMode.HALF_UP))
        .grouped(10).toList.map(_.mkString("\t")).mkString("\n"))
    }
  }
}
