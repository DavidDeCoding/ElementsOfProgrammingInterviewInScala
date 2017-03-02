import scala.annotation.tailrec

/**
  * Created by daviddecoding on 2/27/17.
  */
object CountBits extends App {

  @tailrec
  def countBits(x: Int, res: Int): Int = {
    if (x == 0) res
    else countBits(x >> 1, res + (x & 1))
  }

  val value: Int = 4
  println(countBits(value, 0))
}
