package example

import org.scalatest._

import scala.io.Source

class Challenge1Spec extends FlatSpec with Matchers {


  "Challenge1" should "convert hex to base64" in {

    val hexStr: String =
      "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

    val output: String =
      "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

    Buffer
      .fromHexEncodedString(hexStr)
      .toBase64EncodedString shouldEqual output
  }
}


class Challenge2Spec extends FlatSpec with Matchers {

  "Challenge2" should "produce XOR combination of two equal-length buffers" in {

    val buff1 = Buffer.fromHexEncodedString("1c0111001f010100061a024b53535009181c")
    val buff2 = Buffer.fromHexEncodedString("686974207468652062756c6c277320657965")

    (buff1 ^ buff2).toHexEncodedString shouldEqual "746865206b696420646f6e277420706c6179"
  }
}

class Challenge3Spec extends FlatSpec with Matchers {

  "Challenge3" should "crack a single-byte XOR cipher" in {

    val ciphertext: Buffer =
      Buffer.fromHexEncodedString("1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")

    SingleByteXORSolver.findMostEnglish(
      SingleByteXORSolver.bruteforce(ciphertext)
    ).plaintext
      .toUTF8String shouldEqual "Cooking MC's like a pound of bacon"
  }
}



// TODO this test takes a while to run
class Challenge4Spec extends FlatSpec with Matchers {
  // "Set1/Challenge4" should "detect single-byte XOR cipher" in {
  ignore should "detect single-byte XOR cipher" in {
    val filename = "src/test/assets/4.txt"
    val hexStrings =
      Source.fromFile(filename).getLines
        .map { Buffer.fromHexEncodedString _ }
        .map { SingleByteXORSolver.bruteforce _ }
        .flatten
        .toList

    SingleByteXORSolver
      .findMostEnglish(hexStrings)
      .plaintext
      .toUTF8String
      .trim shouldEqual "Now that the party is jumping"
  }
}



class Challenge5Spec extends FlatSpec with Matchers {
  "Set1/Challenge5" should "implement repeating-key XOR" in {

    val plaintext =
      Buffer.fromUTF8String("Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal")

    val ciphertext =
      "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

    Ciphers.repeatingKeyXOR(plaintext, Buffer.fromUTF8String("ICE"))
      .toHexEncodedString shouldEqual ciphertext
  }
}


class Challenge6Spec extends FlatSpec with Matchers {

  val file =
    Source.fromFile("src/test/assets/6.txt").getLines.mkString

  val ciphertext =
    Buffer.fromBase64EncodedString(file)

  "Challenge6" should "compute Hamming distance" in {
    val one = Buffer.fromUTF8String("this is a test")
    val two = Buffer.fromUTF8String("wokka wokka!!!")
    (one hammingDistance two) shouldEqual 37
  }

  "Challenge6" should "produce scores for keysize guesses" in {
    VigenereSolver
      .scoreKeySizeGuess(ciphertext, 5) shouldBe 3.2271777003484323
  }

  "Challenge6" should "find likely keysizes" in {
    val keysizes =
      Range(2, 41)

    VigenereSolver.findBestKeySizes(ciphertext, keysizes).head shouldBe 29
  }

  "Challenge6" should "crack repeating-key XOR" in {
    VigenereSolver
      .solve(ciphertext, 1)
      .toUTF8String
      .take(33) shouldEqual "I'm back and I'm ringin' the bell"
  }
}
