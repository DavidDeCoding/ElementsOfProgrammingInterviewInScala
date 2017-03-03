/**
  * Created by daviddecoding on 3/1/17.
  */
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
