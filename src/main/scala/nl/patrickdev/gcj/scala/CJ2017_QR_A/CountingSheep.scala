package nl.patrickdev.gcj.scala.CJ2017_QR_A

import java.io._

object CountingSheep extends BaseSolution {
  override def inputFile = "src/main/scala/nl/patrickdev/gcj/scala/CJ2017_QR_A/A-large-practice.in"

  override def solve: Any = {
    val N = in.nextInt
    var seen: Array[Boolean] = new Array[Boolean](10)

    for (a <- 1 to 100) {
      var decN = a * N
      while (decN > 0) {
        seen(decN % 10) = true
        decN /= 10
      }

      if (seen.forall(_ == true)) {
        return a * N
      }
    }

    "INSOMNIA"
  }
}

abstract class BaseSolution extends App {
  def inputFile: String

  def solve: Any

  val in = new In(inputFile)
  val out = new Out(inputFile.replace(".in", ".out"))

  for (i <- 1 to in.nextInt) {
    out.add(s"Case #$i: $solve")
  }
  out.write()

  class In(inputFile: String) {
    private val br: BufferedReader = new BufferedReader(new FileReader(inputFile))
    private var buffer: String = ""

    def nextLine: String = {
      buffer = ""
      br.readLine()
    }

    def nextWord: String = {
      if (buffer.equals("")) {
        buffer = nextLine
      }

      buffer.split(" ", 2) match {
        case Array(word, tail) =>
          buffer = tail
          word
        case Array(word) =>
          buffer = ""
          word
      }
    }

    def nextInt: Int = Integer.parseInt(nextWord)
  }

  class Out(outputFile: String) {
    val strBuilder = new StringBuilder

    def add(str: Object): Unit = {
      strBuilder.append(str.toString)
      strBuilder.append("\n")
      println(str.toString)
    }

    def write(): Unit = {
      val file = new File(outputFile)
      if (file.exists) {
        file.delete()
        file.createNewFile()
      }

      val bw = new BufferedWriter(new FileWriter(file.getAbsoluteFile))
      bw.write(strBuilder.toString)
      bw.close()
    }
  }

}