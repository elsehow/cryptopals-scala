package example

object Utils {

  /** Score how "English" a buffer seems (higher is more English). */
  def scoreEnglishText (input: Buffer): Double = {
    // english char frequencies
    val frequency = Array( ' ' -> .11504, 'a' -> .07227, 'b' -> .01320, 'c' ->
                            .02462, 'd' -> .03764, 'e' -> .11241, 'f' -> .01972, 'g' -> .01783, 'h' ->
                            .05393, 'i' -> .06165, 'j' -> .00135, 'k' -> .00683, 'l' -> .03562, 'm' ->
                            .02129, 'n' -> .05973, 'o' -> .06643, 'p' -> .01707, 'q' -> .00084, 'r' ->
                            .05298, 's' -> .05599, 't' -> .08014, 'u' -> .02441, 'v' -> .00865, 'w' ->
                            .02089, 'x' -> .00133, 'y' -> .01747, 'z' -> .00065).sortBy(_._2).reverse
    // convert input to lowercase
    val str = input.toUTF8String.toLowerCase
    // hackily score phrases
    // TODO  stop this madness
    var freq = collection.mutable.Map(frequency.toSeq: _*)
    freq = freq.map(c ⇒ (c._1, c._2 * str.length))
    var err = 0.0
    for (i ← 'a' to 'z') {
      freq(i) = freq(i) - str.count(_ == i)
      err += Math.pow(freq(i), 2)
    }
    freq(' ') = freq(' ') - str.count(_ == ' ')
    err += Math.pow(freq(' '), 2)
    err = Math.sqrt(err)

    for (i ← str) {
      if (i != ' ' && !(i >= 'A' && i <= 'Z' || i >= 'a' && i <= 'z')) {
        err += 20
      }
      if (i > 126 || i < 32) {
        err += 100
      }
    }
    err
  }


  /** Returns a lazy (infinite!) stream of repetitions of the items in seq. */
  def cycle[T](seq: Seq[T]): Stream[T] = {
    assert(seq.nonEmpty, "Cannot cycle over an empty sequence!")
    Stream.continually(seq).flatten
  }

  def cycleKey (key: Buffer, text: Buffer): Buffer =
    Buffer(Utils.cycle(key.bytes).take(text.length).force.map {_.toByte}.toArray)


  /** XOR decrypt */
  def decrypt (key: Buffer, ciphertext: Buffer): Buffer =
    (ciphertext ^ Utils.cycleKey(key, ciphertext))


}
