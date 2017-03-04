import scala.annotation.tailrec

/**
  * Created by daviddecoding on 3/3/17.
  */
object ParityOfAWord extends App {

  @tailrec
  def parity(x: Long, res: Int): Int = {
    if (x == 0)
      if (res % 2 == 0) 0
      else 1

    else {
      parity(x >> 1, res + (x & 1).asInstanceOf[Int])
    }
  }

  val x = 3l
  println(parity(x, 0))
}
object ParityOfAWord2 extends App {

  /**
    * The idea is parity (bits) = parity (bits_first_half XOR bits_second_half)
    * Recursively we can achieve all parities.
    */
  def parity(x: Long, shift: Int): Int = {
    if (shift == 0) (x & 0x1).asInstanceOf[Int]
    else parity(x ^ (x >>> shift), shift / 2)
  }

  val x = 3l
  println(parity(x, 32))

} // TODO Not working
object CountBits extends App {

  @tailrec
  def countBits(x: Int, res: Int): Int = {
    if (x == 0) res
    else countBits(x >> 1, res + (x & 1))
  }

  val value: Int = 4
  println(countBits(value, 0))
}
object ClosestIntegerWithSameWeight extends App {

  def find(x: Int, idx: Int = 0): Int = {
    if (idx >= 64) -1
    else {
      if (((x >> idx) & 1) == ((x >> (idx + 1)) & 1)) find(x, idx + 1)
      else x ^ ((1 << (idx + 1)) | (1 << idx))
    }
  }

  val x = 3
  println(find(x))
}
object Swap extends App {

  def swap(x: Long, i: Int, j: Int): Long = {

    if (((x >>> i) & 1) != ((x >>> j) & 1)) x ^ ((1 << i) | (1 << j))
    else x
  }

  val x = 3l
  println(swap(x, 0, 1))
}
object ReverseBits extends App {

  // 00 -> 00, 01 -> 10, 10 -> 01, 11 -> 11
  val cache = Array(0, 2, 1, 3)
  val wordsize = 2
  val bitmask = 3 // 11

  // 11 11 11 11
  def reverse(x: Int): Int = {
    cache((x >> (wordsize * 3)) & bitmask) |
      cache((x >> (wordsize * 2)) & bitmask) << wordsize |
      cache((x >> (wordsize * 1)) & bitmask) << (2 * wordsize) |
      cache(x & bitmask) << (3 * wordsize)
  }

  val x = 1
  println(reverse(x))
}
object MultiplyXandY extends App {

  // Bitwise multiplication
  // For every 1 in x, we add (1 * (y << 1)) with sum
  def multiply(x: Int, y: Int): Int = {
    var mul = 0
    var temp_x = x
    var temp_y = y

    while (temp_x != 0) {

      if ((temp_x & 1) != 0) {
        mul = mul | add(mul, temp_y)
      }
      temp_x = temp_x >>> 1
      temp_y = temp_y << 1
    }

    mul
  }

  // Bitwise add
  def add(x: Int, y: Int): Int = {
    var carryin = 0
    var k = 1
    var sum = 0
    var temp_x = x
    var temp_y = y

    while (temp_x != 0 || temp_y != 0) {

      val xk = x & k
      val yk = y & k
      val carryout = (xk & yk) | (xk & carryin) | (yk & carryin)

      sum = sum | (xk ^ yk ^ carryin)

      carryin = carryout << 1
      temp_x = temp_x >>> 1
      temp_y = temp_y >>> 1
      k = k << 1
    }

    sum
  }

  val x = 2
  val y = 2
  println(multiply(x, y))
}
object DivideXandY extends App {


  def divide(x: Int, y: Int): Int = {
    def divideOne(x: Int, y: Int, power: Int): (Int, Int, Int) = {
      if (y > x) divideOne(x, y >> 1, power - 1)
      else (1 << power, power, y)
    }

    def divide(x: Int, y: Int, res: Int, power: Int): Int = {
      if (x >= y) {
        val (new_res, new_power, new_y) = divideOne(x, y, power)
        divide(x - new_y, y, res + new_res, new_power)

      } else res
    }

    divide(x, y, 0, 32)
  }



  val x = 6
  val y = 3
  println(divide(x, y))
}
object Palindrome extends App {

  // Length
  def numOfDigits(x: Int): Int = Math.floor(Math.log10(x)).asInstanceOf[Int] + 1

  // Get LSB
  def lsb(x: Int): Int = x % 10

  // Get MSB
  def msb(x: Int, noOfDigits: Int): Int =
    (x / Math.pow(10, noOfDigits - 1)).asInstanceOf[Int]

  // Remove LSB
  def removeLsb(x: Int): Int = x / 10

  // Remove MSB
  def removeMsb(x: Int, noOfDigits: Int): Int =
    (x % Math.pow(10, noOfDigits - 1)).asInstanceOf[Int]

  def palindromeWithNoOfDigits(x: Int, noOfDigits: Int): Boolean = {
    if (noOfDigits <= 1) true
    else if (lsb(x) != msb(x, noOfDigits)) false
    else
      palindromeWithNoOfDigits(
        removeMsb(
          removeLsb(x),
          noOfDigits),
        noOfDigits - 2)
  }

  def palindrome(x: Int): Boolean = palindromeWithNoOfDigits(x, numOfDigits(x))

  val no = 21
  println(palindrome(no))
}
object RectangleIntersection extends App {
} // TODO Not Done