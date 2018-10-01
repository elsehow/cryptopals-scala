package example



object Ciphers {

  /** Encrypt plaintext with key using a repeating-keyXOR.
    *
    * In repeating-key XOR, you'll sequentially apply each byte of the key; the
    * first byte of plaintext will be XOR'd against I, the next C, the next E,
    * then I again for the 4th byte, and so on. */
  def repeatingKeyXOR (plaintext: Buffer, key: Buffer): Buffer = {

    val encryptionKey: Buffer =
      Utils.cycleKey(key, plaintext)

    Buffer(
      (plaintext.bytes zip encryptionKey.bytes)
        .map { case(x: Byte, y: Byte) => x ^ y}
        .map { _.toByte }
    )
  }


  def decryptAES128ECB (key: Buffer, ciphertext: Buffer): Buffer = {
    import javax.crypto.Cipher
    import java.security.Key
    import javax.crypto.spec.SecretKeySpec

    val decKey =
      new SecretKeySpec(key.bytes, "AES")

    val cipher = Cipher.getInstance("AES/ECB/NoPadding")
    cipher.init(Cipher.DECRYPT_MODE, decKey)
    return Buffer(cipher.doFinal(ciphertext.bytes))
  }

}
