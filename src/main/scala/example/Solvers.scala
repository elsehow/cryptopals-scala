package example

object RepeatingSizeXORSolver {
  /*

   xxxyyyzzz

   break the ciphertext into blocks of KEYSIZE length.

   x x x
   y y y
   z z z

   now transpose the blocks: make a block that is the first byte of every
   block, and a block that is the second byte of every block, and so on. solve
   each block as if it was single-character XOR. for each block, the
   most plausible single-byte XOR key is the
   repeating-key XOR key byte for that block. put them together and you have
   the key.


   | x | x x
   | y | y y
   | z | z z

   |
   | find single-byte xor key
   |
   \ /
   .
   a  b  c     <- repeating-byte XOR key.

   */


  /** Score key size guess using Hamming distance of adjacent blocks in the
    * ciphertext.*/
  def scoreKeySizeGuess (text: Buffer, keySizeGuess: Integer): Double = {
    val pairedSlices =
      text
        .bytes
        .grouped(keySizeGuess)
        .filter { _.length == keySizeGuess} // chop off weird-sized end bits
        .grouped(2) // pair em up
        .filter { _.length == 2} // chop off unpaired buffers
        .toList

    val distances =
      pairedSlices
        .map { case(x: List[List[Byte]]) =>
          Buffer(x.head) hammingDistance Buffer(x.tail.head) }

    val averageDistance =
      distances
        .reduce { _ + _ }
        .toDouble / distances.length

    // normalize the score by the sliceSize
    averageDistance / keySizeGuess
  }


  def transposedBlocks (keySize: Integer, text: Buffer): Seq[Buffer] =
    text
      .bytes
      .grouped(keySize)
  // filter out stubs at the end
  // TODO should we be padding end instead?
      .filter { _.length == keySize}
      .toList
      .transpose
      .map { _.toArray }
      .map { Buffer(_) }

  def singleByteXORSolutionFor (block: Buffer): Int = {
    val keysAndTexts: Seq[(Int, Buffer)] =
      Utils.bruteforceXORCipher(block)
    Utils.findMostEnglish(keysAndTexts)._1
  }

  def findCandidateKey (transposedBlocks: Seq[Buffer]): Buffer = {
    Buffer(
      transposedBlocks
        .map { singleByteXORSolutionFor _ }
        .map { _.toByte }
        .toArray
    )
  }

  def decrypt (key: Buffer, ciphertext: Buffer): Buffer =
    (ciphertext ^ Utils.cycleKey(key, ciphertext))


  def tryAtKeySize (keySize: Integer, ciphertext: Buffer): Buffer = {
    val transposed = transposedBlocks(keySize, ciphertext)
    val candidateKey = findCandidateKey(transposed)
    decrypt(candidateKey, ciphertext)
  }

  def findBestKeySizes (ciphertext: Buffer, candidateKeysizes: Seq[Int]): Seq[Int] =
    candidateKeysizes
      .map { scoreKeySizeGuess(ciphertext, _) }
      .zip { candidateKeysizes }
      .sortWith { _._1 < _._1 }
      .map { _._2 }



  def solve (ciphertext: Buffer, nKeySizesToTry: Int): Buffer = {

    val decryptions =
      List(29, 40, 25, 9, 15)
        .take(nKeySizesToTry)
        .map { tryAtKeySize(_, ciphertext) }

    val scores =
      decryptions
        .map { Utils.scoreEnglishText _ }

    val bestGuess =
      (scores zip decryptions)
        .sortWith( _._1 < _._1 )
        .head
        ._2

    bestGuess
  }

}
