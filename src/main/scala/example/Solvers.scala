package example

case class Solution (
  key: Int,
  plaintext: Buffer
)


object SingleByteXORSolver {

  /** Generate all possible plaintexts */
  def bruteforce (buffer: Buffer): Seq[Solution] = {

    def bufferOf (byte: Byte, length: Integer): Buffer =
      new Buffer(Array.fill[Byte](length)(byte))

    def tryDecryptAgainst(buff: Buffer, byte: Byte) =
      buffer ^ bufferOf(byte, buff.length)

    val keys =
      new Range(0,255, 1)

    val decryptions =
      keys
        .map { _.toByte }
        .map { tryDecryptAgainst(buffer, _) }

    (keys zip decryptions)
    .map { case (k: Int, m: Buffer) => Solution(k, m)}
  }


  /** Find the most probably English buffer  */
  def findMostEnglish (keysAndBuffers: Seq[Solution]): Solution = {

    val scores =
      keysAndBuffers
        .map { _.plaintext }
        .map { Utils.scoreEnglishText _ }

    val best =
      (scores zip keysAndBuffers)
        .sortWith( _._1 < _._1 )
        .head
        ._2

    best
  }

  /** Find the most probable key for the ciphertext. */
  def bestKey (ciphertext: Buffer): Int = {
    val keysAndTexts: Seq[Solution] =
      bruteforce(ciphertext)
    findMostEnglish(keysAndTexts).key
  }


}

object VigenereSolver {
  /*

   Overall strategy:

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


  /** Score how likely a given key size is to be the real keysize. (This score is
    * relative: it can help you compare the likelihood of different keysizes).*/
  def scoreKeySizeGuess (text: Buffer, keySizeGuess: Integer): Double = {

    // evenly slice `text` into chunks of size `keySizeGuess`
    val allSlices: Seq[Array[Byte]] =
      text
        .bytes
        .grouped(keySizeGuess)
        .filter { _.length == keySizeGuess} // chop off weird-sized end bits
        .toList

    // pair adjacent chunks
    val pairedSlices: Seq[Seq[Array[Byte]]] =
      allSlices
        .grouped(2) // pair em up
        .filter { _.length == 2} // chop off unpaired buffers
        .toList

    // find the distances between each pair of adjacent chunks
    val distances =
      pairedSlices
        .map { x =>
          Buffer(x.head) hammingDistance Buffer(x.tail.head) }

    // get the average distance
    val averageDistance =
      distances
        .reduce { _ + _ }
        .toDouble / distances.length

    // and noramlzie it
    averageDistance / keySizeGuess
  }

  /** Rank possible keysizes in order of how likely they are to be the real keysize. */
  def findBestKeySizes (ciphertext: Buffer, candidateKeysizes: Seq[Int]): Seq[Int] =
    candidateKeysizes
      .map { scoreKeySizeGuess(ciphertext, _) }
      .zip { candidateKeysizes }
      .sortWith { _._1 < _._1 }
      .map { _._2 }


  /** Split text into blocks of keySize, then transpose the blocks */
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


  /** Construct a key guess by finding the best single-byte XOR solution for each
    * of the transposed blocks. */
  def findCandidateKey (transposedBlocks: Seq[Buffer]): Buffer = {
    Buffer(
      transposedBlocks
        .map { SingleByteXORSolver.bestKey _ }
        .map { _.toByte }
        .toArray
    )
  }

  /** Find most likely plaintext for a given keySize. */
  def tryAtKeySize (keySize: Integer, ciphertext: Buffer): Buffer = {
    val transposed = transposedBlocks(keySize, ciphertext)
    val candidateKey = findCandidateKey(transposed)
    Utils.decrypt(candidateKey, ciphertext)
  }


  /** Find the most likely solution to a fixed-length XOR cipher */
  def solve (
    ciphertext: Buffer,
    nKeySizesToTry: Int,
    candidateKeysizes: Seq[Int] = Range(2,41)
  ): Buffer = {

    val decryptions =
      findBestKeySizes(ciphertext, candidateKeysizes)
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
