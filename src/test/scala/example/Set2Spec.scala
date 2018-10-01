package example

import org.scalatest._

import scala.io.Source

class Challenge9Spec extends FlatSpec with Matchers {
  "Challenge9" should "implement PKCS#7 padding" in {

    val yellowSub = Buffer.fromUTF8String("YELLOW SUBMARINE")
    yellowSub.length shouldEqual 16

    val padded =
      yellowSub.padPKCS7(20)

    padded.toUTF8String shouldEqual "YELLOW SUBMARINE\u0004\u0004\u0004\u0004"
  }
}


class Challenge10Spec extends FlatSpec with Matchers {
  "Challenge10" should "implement CBC mode" in {
    1 shouldEqual 1
  }
}
