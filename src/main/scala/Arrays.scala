import java.util

import scala.annotation.tailrec
import scala.util.Random

object DutchNationalFlag2 extends App {
  private def swap(x: Int, y: Int, arr: Array[Int]) = {
    val temp = arr(x)
    arr(x) = arr(y)
    arr(y) = temp
  }


  def separateLessEqualGreater(less: Int, equal: Int, greater: Int, arr: Array[Int], value: Int): Array[Int] = {
    if (greater < equal) arr
    else {
      if      (arr(equal) == value) separateLessEqualGreater(less, equal + 1, greater, arr, value)
      else if (arr(equal) > value) {
        swap(equal, greater, arr)
        separateLessEqualGreater(less, equal, greater - 1, arr, value)
      } else {
        swap(equal, less, arr)
        separateLessEqualGreater(less + 1, equal + 1, greater, arr, value)
      }
    }
  }

  val arr1: Array[Int] = Array(1, 2, 3, 2, 1)
  print(util.Arrays.toString(arr1) + " -> ")
  val res1: Array[Int] = separateLessEqualGreater(0, 0, 4, arr1, 3)
  print(util.Arrays.toString(res1))

  println()
  val arr2: Array[Int] = Array(1, 2, 3, 6, 5)
  print(util.Arrays.toString(arr2) + " -> ")
  val res2: Array[Int] = separateLessEqualGreater(0, 0, 4, arr2, 3)
  print(util.Arrays.toString(res2))

  println()
  val arr3: Array[Int] = Array(1, 2, 3, 4, 3)
  print(util.Arrays.toString(arr3) + " -> ")
  val res3: Array[Int] = separateLessEqualGreater(0, 0, 4, arr3, 3)
  print(util.Arrays.toString(res3))
}
object DutchNationalFlag1 extends App {

  private def swap(x: Int, y: Int, arr: Array[Int]) = {
    val temp = arr(x)
    arr(x) = arr(y)
    arr(y) = temp
  }

  def separateLessEqualGreater(arr: Array[Int], value: Int): Array[Int] = {

    def bringLessToFront(less: Int, current: Int, arr: Array[Int], value: Int): Array[Int] = {
      if (current >= arr.length) arr
      else {
        if (arr(current) < value) {
          swap(less, current, arr)
          bringLessToFront(less + 1, current + 1, arr, value)
        } else {
          bringLessToFront(less, current + 1, arr, value)
        }
      }
    }

    def sendGreaterToBack(current: Int, greater: Int, arr: Array[Int], value: Int): Array[Int] = {
      if (greater <= current) arr
      else {
        if (arr(current) > value) {
          swap(current, greater, arr)
          sendGreaterToBack(current, greater - 1, arr, value)
        } else {
          sendGreaterToBack(current + 1, greater, arr, value)
        }
      }
    }

    bringLessToFront(0, 0,
      sendGreaterToBack(0, arr.length - 1,
        arr, value),
      value)
  }

  val arr1: Array[Int] = Array(1, 2, 3, 2, 1)
  print(util.Arrays.toString(arr1) + " -> ")
  val res1: Array[Int] = separateLessEqualGreater(arr1, 3)
  print(util.Arrays.toString(res1))

  println()
  val arr2: Array[Int] = Array(1, 2, 3, 6, 5)
  print(util.Arrays.toString(arr2) + " -> ")
  val res2: Array[Int] = separateLessEqualGreater(arr2, 3)
  print(util.Arrays.toString(res2))

  println()
  val arr3: Array[Int] = Array(1, 2, 3, 4, 3)
  print(util.Arrays.toString(arr3) + " -> ")
  val res3: Array[Int] = separateLessEqualGreater(arr3, 3)
  print(util.Arrays.toString(res3))

}
object AdvancingAnIntArray extends App {

  def canAdvanceToEnd(arr: Array[Int], idx: Int, furtherAdvance: Int): Boolean = {
    if (furtherAdvance < idx) return false

    if (arr.length - 1 == idx) {
      if (furtherAdvance >= arr.length - 1) return true
      else return false
    }

    val furtherAdvanceForIdx =
      Math.max(
        furtherAdvance,
        arr(idx) + idx)
    canAdvanceToEnd(arr, idx + 1, furtherAdvanceForIdx)
  }

  val arr1 = Array(3, 3, 1, 0, 2, 0, 1)
  println(canAdvanceToEnd(arr1, 0, 0))

  val arr2 = Array(3, 2, 0, 0, 2, 0, 1)
  println(canAdvanceToEnd(arr2, 0, 0))
}
object IncrementArbitaryPrecisionInteger extends App {

  def increment(arr: Array[Int]): Array[Int] = {
    val lastIdx = arr.length - 1
    arr(lastIdx) += 1

    for (idx <- lastIdx until 0 by -1) {
      if (arr(idx) == 10) {
        arr(idx) = 0
        arr(idx - 1) += 1
      }
    }

    if (arr(0) == 10) {
      arr(0) = 0

      val newarr = new Array[Int](arr.length + 1)
      newarr(0) = 1
      for (idx <- 0 to lastIdx) newarr(idx + 1) = arr(idx)

      newarr

    } else arr
  }

  val intArr1 = Array(1, 3, 5)
  println(util.Arrays.toString(increment(intArr1)))

  val intArr2 = Array(1, 3, 9)
  println(util.Arrays.toString(increment(intArr2)))

  val intArr3 = Array(9, 9, 9)
  println(util.Arrays.toString(increment(intArr3)))
}
object MultiplyArbitaryPrecisionIntegers extends App {

  def multiply(x: Array[Int], y: Array[Int]): Array[Int] = {
    val res: Array[Int] = new Array[Int](x.length + y.length)

    for (i <- x.length - 1 to 0 by -1) {
      for (j <- y.length - 1 to 0 by -1) {
        res(i + j + 1) += x(i) * y(j)
        res(i + j) += res(i + j + 1) / 10
        res(i + j + 1) %= 10
      }
    }

    res
  }

  val x = Array(1, 2)
  val y = Array(1, 2)
  println(util.Arrays.toString(multiply(x, y)))
}
object OddEvenSeparation extends App {

  private def swap(x: Int, y: Int, arr: Array[Int]) = {
    val temp = arr(x)
    arr(x) = arr(y)
    arr(y) = temp
  }

  def separate(lo: Int, hi: Int, arr: Array[Int]): Array[Int] = {
    if (hi <= lo) arr
    else {
      if      (arr(lo) % 2 == 0) separate(lo + 1, hi, arr)
      else if (arr(hi) % 2 != 0) separate(lo, hi - 1, arr)
      else {
        swap(lo, hi, arr)
        separate(lo + 1, hi - 1, arr)
      }
    }
  }

  val arr: Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val res: Array[Int] = separate(0, 9, arr)
  println(util.Arrays.toString(res))
}
object FindAllPrimeNumbers extends App {

  def seive(n: Int): List[Int] = {
    var primes: List[Int] = Nil


    val size = Math.floor(0.5 * (n - 3)).asInstanceOf[Int] + 1
    val isPrime: Array[Boolean] = new Array(size)
    for (i <- 0 until size) isPrime(i) = true

    if (n > 2) primes =  primes ::: List(2)

    // 2i * 3
    // 4i^2 + 6i + 9

    for (i <- 0 until size) {
      if (isPrime(i)) {
        val idx = (2 * i) + 3
        primes = primes ::: List(idx)

        for (j <- (2 * (i * i)) + (6 * i) + 3 to size by ((2 * i) + 3)) {
          isPrime(j) = false
        }
      }

    }
    primes
  }

  val n = 10
  println(seive(n))
}
object BuyAndSellStockOnce extends App {

  def findMaxProfit(arr: Array[Int], idx: Int, maxProfit: Int, minPriceSoFar: Int): Int = {
    if (idx == arr.length) maxProfit
    else {
      val newMaxProfit = Math.max(maxProfit, arr(idx) - minPriceSoFar)
      val newMinSoFar = Math.min(minPriceSoFar, arr(idx))

      findMaxProfit(arr, idx + 1, newMaxProfit, newMinSoFar)
    }
  }

  val arr = Array(310, 315, 275, 295, 260, 270, 290, 230, 255, 250)
  println(findMaxProfit(arr, 0, Integer.MIN_VALUE, Integer.MAX_VALUE))
}
// TODO The answer is wrong!
object BuyAndSellStockTwice extends App {

  def findMaxProfit(prices: Array[Int]): Int = {
    def findMaxProfitForward(arr: Array[Int], profits: Array[Int], idx: Int, maxProfit: Int, minPriceSoFar: Int): Int = {
      if (idx == arr.length - 1) return maxProfit

      val newMaxProfit = Math.max(maxProfit, arr(idx) - minPriceSoFar)
      val newMinPriceSoFar = Math.min(minPriceSoFar, arr(idx))
      profits(idx) = newMaxProfit
      findMaxProfitForward(arr, profits, idx + 1, newMaxProfit, newMinPriceSoFar)
    }

    def findMaxProfitBackward(arr: Array[Int], profits: Array[Int], idx: Int, maxProfit: Int, maxPriceSoFar: Int): Int = {
      if (idx == arr.length - 1) return maxProfit

      val newMaxProfit = Math.max(maxProfit, maxPriceSoFar - arr(idx) + profits(idx - 1))
      val newMaxPriceSofar = Math.max(maxPriceSoFar, arr(idx))
      profits(idx) = newMaxProfit
      findMaxProfitBackward(arr, profits, idx + 1, newMaxProfit, newMaxPriceSofar)
    }

    val size = prices.length
    val profits: Array[Int] = new Array(size)
    val maxTotalProfit = findMaxProfitForward(prices, profits, 1, 0, prices(0))

    val lastIdx = prices.length - 1
    findMaxProfitBackward(prices, profits, lastIdx - 1, maxTotalProfit, prices(lastIdx))
  }

  val prices = Array(12, 11, 13, 9, 12, 8, 14, 13, 15)
  println(findMaxProfit(prices))
}
object DeleteDuplicates extends App {

  def dedup(arr: Array[Int], idx: Int, value: Int, writeIdx: Int): Array[Int] = {
    if      (idx == arr.length) arr
    else if (arr(idx) == value) dedup(arr, idx + 1, value, writeIdx)
    else {
      arr(writeIdx) = arr(idx)
      dedup(arr, idx + 1, arr(idx), writeIdx + 1)
    }
  }

  val arr1 = Array(1, 2, 3)
  println(util.Arrays.toString(dedup(arr1, 1, arr1(0), 1)))

  val arr2 = Array(1, 1, 2, 2, 3)
  println(util.Arrays.toString(dedup(arr2, 1, arr2(0), 1)))
}
object NextPermutation extends App {

  def nextPermutation(perm: Array[Int]): Array[Int] = {

    @tailrec
    def findK(perm: Array[Int], k: Int): Int = {
      if      (k == -1) -1
      else if (perm(k) < perm(k + 1)) k
      else findK(perm, k - 1)
    }

    @tailrec
    def findL(perm: Array[Int], i: Int, value: Int, l: Int): Int = {
      if (i == -1) l
      else if (perm(i) > value && (l == -1 || perm(i) < perm(l))) findL(perm, i - 1, value, i)
      else findL(perm, i - 1, value, l)
    }

    @tailrec
    def reverse(perm: Array[Int], left: Int, right: Int): Array[Int] = {
      if (right <= left) perm
      else {
        swap(perm, left, right)
        reverse(perm, left + 1, right - 1)
      }
    }

    def swap(perm: Array[Int], i: Int, j: Int): Array[Int] = {
      val temp = perm(i)
      perm(i) = perm(j)
      perm(j) = temp
      perm
    }

    findK(perm, perm.size - 2) match {
      case -1 => perm
      case k =>
        val l = findL(perm, perm.size - 1, perm(k), -1)
        swap(perm, k, l)
        reverse(perm, k + 1, perm.size - 1)
    }
  }


  val perm = Array(5, 1, 3, 2)
  println(util.Arrays.toString(nextPermutation(perm)))

}
object ComputeRandomSubset extends App {

  def random(start: Int, end: Int): Int = {
    start + new Random().nextInt((end - start) + 1)
  }

  def compute(n: Int, k: Int, i: Int, changedElements: Map[Int, Int]): Seq[Int] = {
    if (i == k) for (k_i <- 0 until k) yield changedElements(k_i)
    else {
      val randIdx = random(i, n - 1)
      if (!changedElements.contains(randIdx) &&
        !changedElements.contains(i))
        compute(n, k, i + 1, changedElements ++ Map(randIdx -> i, i -> randIdx))
      else if (!changedElements.contains(randIdx) &&
        changedElements.contains(i))
        compute(n, k, i + 1,
          changedElements ++ Map(
            randIdx -> changedElements(i),
            changedElements(i) -> i))
      else if (changedElements.contains(randIdx) &&
        !changedElements.contains(i))
        compute(n, k, i + 1,
          changedElements ++ Map(
            i -> changedElements(randIdx),
            changedElements(randIdx) -> randIdx))
      else
        compute(n, k, i + 1, changedElements ++ Map(
          i -> changedElements(randIdx),
          randIdx -> changedElements(i)))
    }
  }

  println(compute(10, 3, 0, Map[Int, Int]()))
}
object SudokuValidationChecker extends App {

  def validate(sudoku: Array[Array[Int]]): Boolean = {
    def hasDuplicate(sudoku: Array[Array[Int]], startRow: Int, endRow: Int, startCol: Int, endCol: Int): Boolean = {
      val isPresent: Array[Boolean] = new Array[Boolean](10)
      for {
        i <- startRow to endRow
        j <- startCol to endCol
      } {
        if (sudoku(i)(j) != 0 && isPresent(sudoku(i)(j))) return true
        else isPresent(sudoku(i)(j)) = true
      }
      false
    }
    val len = sudoku.length

    for (i <- 0 to len) {
      if (hasDuplicate(sudoku, i, i + 1, 0, len)) return false
    }

    for (i <- 0 to len) {
      if (hasDuplicate(sudoku, 0, len, i, i + 1)) return false
    }

    val regionSize = Math.sqrt(len).asInstanceOf[Int]
    for {
      i <- 0 to regionSize
      j <- 0 to regionSize
    } if (hasDuplicate(sudoku, i * regionSize, (i + 1) * regionSize, j * regionSize, (j + 1) * regionSize)) return false

    true
  }




}
object SpiralTraversal extends App {

  def traverse(matrix: Array[Array[Int]], offset: Int, list: List[Int]): List[Int] = {
    if (offset == Math.ceil(0.5 * matrix.length)) return list

    if (offset == matrix.length - offset - 1) {
      return traverse(matrix, offset + 1, list ::: List(matrix(offset)(offset)))
    }

    var newList: List[Int] = list
    for (j <- offset until matrix.length - offset - 1)       newList = newList ::: List(matrix(offset)(j))
    for (i <- offset until matrix.length - offset - 1)       newList = newList ::: List(matrix(i)(matrix.length - offset - 1))
    for (j <- matrix.length - offset - 1 until offset by -1) newList = newList ::: List(matrix(matrix.length - offset - 1)(j))
    for (i <- matrix.length - offset - 1 until offset by -1) newList = newList ::: List(matrix(i)(offset))
    traverse(matrix, offset + 1, newList)
  }

  val matrix1: Array[Array[Int]] =
    Array(
      Array(1, 2, 3),
      Array(4, 5, 6),
      Array(7, 8, 9)
    )
  println(traverse(matrix1, 0, List()))

  val matrix2: Array[Array[Int]] =
    Array(
      Array(1, 2, 3, 4),
      Array(5, 6, 7, 8),
      Array(9, 10, 11, 12),
      Array(13, 14, 15, 16)
    )
  println(traverse(matrix2, 0, List()))
}
object Rotate2DArray extends App {

  def rotate(arr: Array[Array[Int]]): Array[Array[Int]] = {
    def rotateLayer(arr: Array[Array[Int]], offset: Int, i: Int): Array[Array[Int]] = {
      if (i == arr.length - 1 - offset) arr
      else {
        val temp1 = arr(arr.length - 1 - i)(offset)
        val temp2 = arr(arr.length - 1 - offset)(arr.length - 1 - i)
        val temp3 = arr(i)(arr.length - 1 - offset)
        val temp4 = arr(offset)(i)

        arr(offset)(i) = temp1
        arr(arr.length - 1 - i)(offset) = temp2
        arr(arr.length - 1 - offset)(arr.length - 1 - i) = temp3
        arr(i)(arr.length - 1 - offset) = temp4


        rotateLayer(arr, offset, i + 1)
      }
    }

    def rotate(arr: Array[Array[Int]], layer: Int): Array[Array[Int]] = {
      if (layer == arr.length / 2) arr
      else {
        rotateLayer(arr, layer, layer)
        rotate(arr, layer + 1)
      }
    }

    rotate(arr, 0)
  }

  val arr: Array[Array[Int]] = Array(
    Array(1, 2, 3, 4),
    Array(5, 6, 7, 8),
    Array(9, 10, 11, 12),
    Array(13, 14, 15, 16)
  )

  val res = rotate(arr)
  for (i <- 0 until res.length) println(util.Arrays.toString(res(i)))
}
object PascalsTriangle extends App {

  def drawTriangle(n: Int): Array[Array[Int]] = {
    val arr: Array[Array[Int]] = Array.ofDim[Int](n, n)

    for {
      i <- 0 until n
      j <- 0 to i
    } {
      arr(i)(j) =
        if (j == 0 || j == i) 1
        else arr(i - 1)(j - 1) + arr(i - 1)(j)
    }

    arr
  }

  val n = 4
  val res = drawTriangle(n)
  for (i <- 1 to n) println(util.Arrays.toString(res(i - 1)))
}