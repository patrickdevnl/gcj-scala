package nl.patrickdev.gcj2018_scala.test

import java.io._

object TestSolution extends BaseSolution {
  override def inputFile = "D:\\Cloud\\programming\\gcj2018_scala\\src\\main\\scala\\nl\\patrickdev\\gcj2018_scala\\test\\sample.in"

  override def solve: Any = {
    in.nextInt * 3
  }
}

abstract class BaseSolution extends App {
  def inputFile: String
  def solve: Any

  val in = new In(inputFile)
  val out = new Out(inputFile.replace(".in", ".out"))

  startLoop()

  def startLoop(): Unit = {
    for (i <- 1 to in.nextInt) {
      out.add(s"Case #$i: ${solve.toString}")
    }

    out.write()
  }

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