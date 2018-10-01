package example

import java.util.Base64

object Buffer {

  def fromHexEncodedString (input: String): Buffer =
    Buffer(BigInt(input, 16).toByteArray)

  def fromUTF8String (input: String): Buffer =
    Buffer(input.getBytes)

  def fromBase64EncodedString (input: String): Buffer =
    Buffer(Base64.getDecoder.decode(input))

}

case class Buffer (bytes: Array[Byte]) {

  /* Properties */

  def length: Integer =
    bytes.length

  /* Utilities */

  def slice (from: Integer, until: Integer): Buffer =
    Buffer(bytes.slice(from, until))

  /* Converters */

  def toUTF8String: String =
    new String(bytes)

  def toBase64EncodedString: String =
    Base64.getEncoder.encodeToString(bytes)

  def toHexEncodedString: String =
    bytes.map("%02x".format(_)).mkString

  /* Comperators */

  /** Takes two equal-length buffers and produces their XOR combination. */
  def ^ (other: Buffer): Buffer = {
    require(bytes.length == other.bytes.length, "Buffers must be equal length")
    new Buffer(
      (bytes zip other.bytes)
        .map { case (a:Byte, b:Byte) => a ^ b }
        .map { _.toByte }
    )
  }

  /** Compute Hamming distance (i.e. the number of differing bits). */
  def hammingDistance (other: Buffer): Integer = {
    require(bytes.length == other.bytes.length, "Buffers must be equal length")

    // how many bits in a buffer are set?
    def numberOfBitsSet(b: Byte) : Int =
      (0 to 7).map((i : Int) => (b >>> i) & 1).sum

    (bytes zip other.bytes)
      .map { case (a:Byte, b:Byte) => a ^ b } // take the xor of the two bytes
      .map { _.toByte }
      .map { numberOfBitsSet _ } // in that xor'd version, how many bits are set?
      .sum // sum all of the comparisons
  }

  /** Score of 1 means all blocks are perfectly unique; less than that indicates
    * repeating blocks */
  def blockUniqueness(
    blockSize: Integer=16
  ): Double = {
    // break into blockSize groups
    val groups =
      this
        .toHexEncodedString
        .grouped(blockSize)
        .toVector
    groups
      .distinct
      .length
      .toDouble / groups.length
  }



}
