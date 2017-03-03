/**
  * Created by daviddecoding on 3/1/17.
  */
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
