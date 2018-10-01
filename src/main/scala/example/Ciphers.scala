package example

object Ciphers {

  /** Encrypt plaintext with key using a repeating-keyXOR.
    *
    * In repeating-key XOR, you'll sequentially apply each byte of the key; the
    * first byte of plaintext will be XOR'd against I, the next C, the next E,
    * then I again for the 4th byte, and so on. */
  def repeatingKeyXOR (plaintext: Buffer, key: Buffer): Buffer = {

    val decryptKey: Buffer =
      Utils.cycleKey(key, plaintext)

    Buffer(
      (plaintext.bytes zip decryptKey.bytes)
        .map { case(x: Byte, y: Byte) => x ^ y}
        .map { _.toByte }
    )
  }

}
