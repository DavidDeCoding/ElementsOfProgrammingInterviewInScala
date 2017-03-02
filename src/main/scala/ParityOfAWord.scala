import scala.annotation.tailrec

/**
  * Created by daviddecoding on 2/27/17.
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

// TODO Not working
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

}
