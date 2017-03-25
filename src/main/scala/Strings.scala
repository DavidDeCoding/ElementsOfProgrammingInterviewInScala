import scala.collection.mutable

/**
  * Created by daviddecoding on 3/24/17.
  */
class Strings {

  // TODO
  object ConvertBase extends App {

    //  def convertBase(no: String, base1: Int, base2: Int): String = {
    //
    //    def convertBaseToDecimal(no: String, idx: Int, base: Int, newno: Int): Int = {
    //      if (idx == no.length) newno
    //      else if (Character.isDigit(no(idx))) convertBaseToDecimal(no, idx + 1, base, (base * newno) + ((no(idx) % base) - '0').asInstanceOf[Char])
    //      else convertBaseToDecimal(no, idx + 1, base, newno + (10 + (no(idx) - 'A')).asInstanceOf[Char])
    //    }
    //
    //    def convertDecimalToBase(no: Int, base: Int, newno: String): String = {
    //      if (no == 0) newno
    //      else convertBaseToDecimal(no, no / base, base, newno)
    //    }
    //  }
  }
  object Mnemonics extends App {

    val phn: Array[List[Char]] =
      Array(
        List(),
        List('A', 'B', 'C'),
        List('D', 'E', 'F'),
        List('G', 'H', 'I'),
        List('J', 'K', 'L'),
        List('M', 'N', 'O'),
        List('P', 'Q', 'R', 'S'),
        List('T', 'U', 'V'),
        List('W', 'X', 'Y', 'Z'),
        List())

    def findAll(str: String): mutable.ListBuffer[String] = {
      def findAll(str: String, partialStr: String, list: mutable.ListBuffer[String], idx: Int): Unit = {
        if (idx == str.length) list += partialStr
        else phn(str(idx) - '0').foreach { c =>
          findAll(str, partialStr + c, list, idx + 1)
        }
      }
      val list = mutable.ListBuffer[String]()
      findAll(str, "", list, 0)
      list
    }

    val no = "123"
    println(findAll(no))


  }
  object ReplaceAndRemove extends App {

    def replaceAndRemove(str: String): String = {
      def removeBAndCountA(str: String, res: String, idx: Int): String = {
        if (idx == str.length) res
        else removeBAndCountA(
          str,
          if (str(idx) != 'b') res + str(idx) else res,
          idx + 1
        )
      }

      def replaceA(str: String, res: String, strIdx: Int): String = {
        if (strIdx == str.length) res
        else if (str(strIdx) == 'a') replaceA(str, res + "dd", strIdx + 1)
        else replaceA(str, res + str(strIdx), strIdx + 1)
      }

      replaceA(
        removeBAndCountA(
          str.toLowerCase, "", 0),
        "", 0)
    }

    val str = "aahnmbana"
    println(replaceAndRemove(str))
  }
  object ReverseEachWord extends App {

    def reverseEachWord(str: String): String = {
      def reverseEachWord(str: String, res: String, left: Int, right: Int): String = {
        if (right == str.length) res + str.substring(left, right).reverse
        else if (str(right) == ' ') reverseEachWord(str, res + str.substring(left, right).reverse + ' ', right + 1, right + 1)
        else reverseEachWord(str, res, left, right + 1)
      }

      reverseEachWord(str, "", 0, 0)
    }

    val str = "apple a day keeps the doctor away"
    println(reverseEachWord(str))
  }
  object RomanToInteger extends App {

    def toInteger(romanInteger: String): Int = {
      val map = Map(
        'I' -> 1,
        'V' -> 5,
        'X' -> 10,
        'L' -> 50,
        'C' -> 100,
        'D' -> 500,
        'M' -> 1000)

      def toInteger(romanInteger: String, idx: Int, sum: Int): Int = {
        if (idx < 0) sum
        else if (map(romanInteger(idx)) < map(romanInteger(idx + 1))) toInteger(romanInteger, idx - 1, sum - map(romanInteger(idx)))
        else toInteger(romanInteger, idx - 1, sum + map(romanInteger(idx)))
      }

      val lastIdx = romanInteger.length - 1
      toInteger(
        romanInteger,
        romanInteger.length - 2,
        map(romanInteger(lastIdx)))
    }

    val romanInteger = "LVIIII"
    println(toInteger(romanInteger))
  }
  object RunLengthEncoding extends App {

    def encode(str: String, res: String, count: Int, index: Int): String = {
      if      (index == str.length)           res + count + str(index - 1)
      else if (str(index) != str(index - 1))  encode(str, res + count + str(index - 1), 1, index + 1)
      else                                    encode(str, res, count + 1, index + 1)
    }

    def decode(str: String, res: String, count: Int, index: Int): String = {
      if      (index == str.length)           append(res, count, str(index - 1))
      else if (str(index).isDigit)            decode(str, res, 10 * count + Integer.parseInt(str(index) + ""), index + 1)
      else                                    decode(str, append(res, count, str(index)), 0, index + 1)
    }

    def append(str: String, count: Int, c: Char): String = {
      if (count == 0) str
      else append(str + c, count - 1, c)
    }


    val str = "aaabbcccc"
    println(encode(str, "", 1, 1))

    val encodedStr = "2a3b1c2d"
    println(decode(encodedStr, "", 0, 0))
  }
  object RabinKarp extends App {

    def toInt(ch: Char): Int = {
      ch - 'a'
    }

    def search(str: String, pattern: String): Int = {
      val base = 26
      if (str.length < pattern.length) -1
      else {
        var str_hash = 0
        var pattern_hash = 0
        var power_hash = 1

        for (i <- 0 until pattern.length) {
          str_hash = (base * str_hash) + toInt(str(i))
          pattern_hash = (base * pattern_hash) + toInt(pattern(i))
          power_hash = if (i == 0) 1 else power_hash * base
        }

        for (i <- pattern.length until str.length) {
          if (str_hash == pattern_hash && pattern == str.substring(i - pattern.length, i)) return i - pattern.length
          else {
            str_hash -= power_hash * toInt(str(i - pattern.length))
            str_hash = base * str_hash + toInt(str(i))
          }
        }

        if (str_hash == pattern_hash && pattern == str.substring(str.length - pattern.length, str.length)) str.length - pattern.length
        else -1
      }
    }

    val str = "ccabacc"
    val pattern = "aba"

    println(search(str, pattern))
  }
}
