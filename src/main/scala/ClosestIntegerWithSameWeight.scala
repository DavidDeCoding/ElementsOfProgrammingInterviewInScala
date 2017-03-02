/**
  * Created by daviddecoding on 2/28/17.
  */
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
