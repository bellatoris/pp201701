package pp201701.hw7test
import pp201701.hw7.Main._
import pp201701.hw7.Data.DataBundle._

object Test extends App {
  def print_result(b:Boolean) : Unit =
    if (b) println("O") else println("X")

  val report1 = get_report(true, 2)
  print_result(pretty_printer(report1) == "<Primes 2>\n * 1-th prime : 3\n * 2-th prime : 5\n")
  println(pretty_printer(report1))

  val report2 = get_report(false, 3)
  print_result(pretty_printer(report2) == "<Pi 3>\n * 1-th digit : 1\n * 2-th digit : 4\n * 3-th digit : 1\n")
  println(pretty_printer(report2))

  val report3 = get_report(true, 21)
  print_result(pretty_printer(report3) == "<Error: Too big input>\n")
  println(pretty_printer(report3))

  val report4 = get_report(true, 20)
  // print_result(pretty_printer(report4) == "<Primes 20>\n * 1-th prime : 3\n * 2-th-prime : 5\n ... \n")
  println(pretty_printer(report4))

  val report5 = get_report(false, 20)
  // print_result(pretty_printer(report5) == "<Pi 20>\n * 1-th digit : 1\n * 2-th digit : 4\n * 3-th digit : 1\n ... \n")
  println(pretty_printer(report5))
}
