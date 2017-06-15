package pp201701.hw7
import pp201701.hw7.Data.DataBundle._
import pp201701.hw7.Data.DynObj._

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

object Main {

  object PiDigit {
    // Pi = 3.1415926535..
    val digits = List(1,4,1,5,9,2,6,5,3,5, 8,9,7,9,3,2,3,8,4,6)
  }

  def get_prefix(m:Int, l:List[Int]) : List[Int] =
    if (m > 0)
      l match {
        case h::t => h::get_prefix(m-1, t)
        case Nil => Nil
      }
      else Nil

  type IntPair = (Int, Int)

  // Primes
  type TitlePrimes = (String, (Int, Primes))

  val primeKeyVal : KeyVal[IntPair] = new KeyVal[IntPair] {
    def get_key(a:IntPair) = a._1.toString() + "-th prime"
    def get_val(a:IntPair) = a._2.toString()
  }

  def primesIter(m:Int) : Iter[(Int, Primes), IntPair] = new Iter[(Int, Primes), IntPair] {
    def getValue(a:(Int, Primes)) : Option[IntPair] =
      if (a._1 > m) None else Some((a._1, a._2.prime))
    def getNext(a:(Int, Primes)) : (Int, Primes) = (a._1 + 1, a._2.getNext)
  }

  def titlePrimesIterable(m:Int) : Iterable[TitlePrimes, IntPair] =
    new Iterable[TitlePrimes, IntPair] {
      def iter(a:TitlePrimes) : Dyn2[Iter, IntPair] = Dyn2(a._2)(primesIter(m))
    }

  def reportPrimes(m:Int) : Report[TitlePrimes] = new Report[TitlePrimes] {
    type A = IntPair
    def title(p:TitlePrimes) = p._1
    def it = titlePrimesIterable(m)
    def keyval = primeKeyVal
  }

  // pi
  val piKeyVal : KeyVal[IntPair] = new KeyVal[IntPair] {
    def get_key(a:IntPair) = a._1.toString() + "-th digit"
    def get_val(a:IntPair) = a._2.toString()
  }

  type TitlePiDigit = (String, (Int, List[Int]))

  implicit val piDigitIter : Iter[(Int, List[Int]), IntPair] = new Iter[(Int, List[Int]), IntPair] {
    def getValue(a:(Int, List[Int])) : Option[IntPair] =
      a._2.headOption match {
        case Some(v) => Some((a._1, v))
        case None => None
      }
    def getNext(a:(Int, List[Int])) : (Int, List[Int]) = (a._1 + 1, a._2.tail)
  }

  val titlePiDigitIterable : Iterable[TitlePiDigit, IntPair] =
    new Iterable[TitlePiDigit, IntPair] {
      def iter(a:TitlePiDigit) : Dyn2[Iter, IntPair] = a._2 // Dyn2(a._2)(piDigitIter)
    }

  implicit val reportPiDigit : Report[TitlePiDigit] = new Report[TitlePiDigit] {
    type A = IntPair
    def title(p:TitlePiDigit) = p._1
    def it = titlePiDigitIterable
    def keyval = piKeyVal
  }

  // Error
  // Or, you can use [new Primes(0)].
  type TitleErrorMsg = (String, List[IntPair])

  implicit val titleErrorMsgIterable : Iterable[TitleErrorMsg, IntPair] =
    new Iterable[TitleErrorMsg, IntPair] {
      def iter(a:TitleErrorMsg) : Dyn2[Iter, IntPair] = a._2 // Dyn2(a._2)(listIter[IntPair])
    }

  implicit val reportErrorMsg : Report[TitleErrorMsg] = new Report[TitleErrorMsg] {
    type A = IntPair
    def title(e:TitleErrorMsg) = e._1
    def it = implicitly[Iterable[TitleErrorMsg, IntPair]]
    def keyval = piKeyVal // give anything
  }

  /* Generate a report that contains n items.
   * If is_prime is true, generate a report for primes that starts from 3.
   * Otherwise, generate a report for Pi digits.
   * If n > 20, generate an error report.
   * Use the "Primes" class in "Data.scala".
   */
  def get_report(is_prime:Boolean, n:Int) : Dyn[Report] =
    if (n > 20) ("<Error: Too big input>", Nil:List[IntPair])
    else if (is_prime) Dyn(("<Primes " + n + ">", (1, new Primes())))(reportPrimes(n))
    else ("<Pi " + n + ">", (1, get_prefix(n, PiDigit.digits)))

  /* Check "Test.scala" for the exact format. */
  def pretty_printer(r:Dyn[Report]) : String = {
    def go[R, A](r:R)(proxyIter:Iterable[R, A])(kv:KeyVal[A]) : String = {
      val it:Dyn2[Iter, A] = proxyIter.iter(r)
      goI(it.d)(it.i)(kv)
    }
    def goI[I, A](xs:I)(proxy:Iter[I, A])(kv:KeyVal[A]) : String =
      proxy.getValue(xs) match {
        case Some(a) => {
          val data_str = " * " + kv.get_key(a) + " : " + kv.get_val(a) + "\n"
          data_str + goI(proxy.getNext(xs))(proxy)(kv)
        }
        case None => ""
      }

    val title : String = r.i.title(r.d) + "\n"
    title + go(r.d)(r.i.it)(r.i.keyval)
  }
}
