package example

import org.scalatest._


class BufferSpec extends FlatSpec with Matchers {
  "Buffer" should "encode and decode Hex strings" in {
    val hexStr: String =
      "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    Buffer
      .fromHexEncodedString(hexStr)
      .toHexEncodedString shouldEqual hexStr
  }
  "Buffer" should "encode and decode base64 strings" in {
    val base64: String =
      "HUIfTQsPAh9PE048GmllH0kcDk4TAQsHThsBFkU2AB4BSWQgVB0dQzNTTmVS"
    Buffer
      .fromBase64EncodedString(base64)
      .toBase64EncodedString shouldEqual base64
  }
}
