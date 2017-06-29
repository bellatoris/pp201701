package pp201701.fin
import pp201701.fin.Data.DataBundle._

/*
 * ** The submitted code should be runnable. Before upload, you MUST check it
      whether Test.scala runs successfully. Otherwise you may get 0 points for
      all problems.

 * ** Compress the 'src' folder only. Don't put .sh files in the zip file.
      You can put the .iml file together, if you feel difficulty for some reasons.

 * ** Use only the features taught in class. Especially, don't use imperative
      features such as var, while, for, return, and so on. You may get deduction
      in your points.

 * ** Do not use equivalent built-in features in Scala. The core logic should be
      implemented by you.

 */

object Fib {
  /*
   Problem 5: Implement a function that generates fibonacci dictionary.
   fib(1) = 1
   fib(2) = 1
   fib(3) = 2
   ..

   Your implementation should be efficient enough to calculate fib(100) in short time.

   Assume n > 0.
   */

  def fibonacci[D](n:Int)(implicit proxy:Dict[D, Int, BigInt]) : D = {
    val empty : D = proxy.empty
    def genfib(m:Int, d:D) : D = {
      if (m > n) d
      else {
        val curd =
          if (m == 1 || m == 2) proxy.add(d, m, BigInt("1"))
          else (proxy.lookup(d, m-1), proxy.lookup(d, m-2)) match {
            case (Some(a), Some(b)) => proxy.add(d, m, a+b)
            case _ => d //error
          }
        genfib(m+1, curd)
      }
    }
    genfib(1, empty)
  }

}
