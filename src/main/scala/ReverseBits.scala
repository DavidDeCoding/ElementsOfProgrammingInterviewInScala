/**
  * Created by daviddecoding on 2/28/17.
  */
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
