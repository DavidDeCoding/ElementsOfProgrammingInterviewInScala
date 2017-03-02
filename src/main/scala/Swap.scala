/**
  * Created by daviddecoding on 2/28/17.
  */
object Swap extends App {

  def swap(x: Long, i: Int, j: Int): Long = {

    if (((x >>> i) & 1) != ((x >>> j) & 1)) x ^ ((1 << i) | (1 << j))
    else x
  }

  val x = 3l
  println(swap(x, 0, 1))
}
