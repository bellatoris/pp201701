package pp201701.proj
import scala.annotation.tailrec
import pp201701.proj.Data.DataBundle._
import scala.collection.immutable._

object Value {
  sealed abstract class Val

  abstract class ConvertToScala[A] {
    def toInt(a:A) : Option[Int]
    def toBool(a:A) : Option[Boolean]
    def toPair(a:A) : Option[(A, A)]
    def isNil(a:A) : Boolean
    def isFun(a:A) : Boolean
  }

  // implicit val valConv : ConvertToScala[Val]
}

object Main {
  import Value._

  class EvalException(val msg: String) extends Exception

  def myeval(e:Expr) : Val = throw new EvalException("Not implemented yet")

  def myeval_memo(e:Expr) : Val = throw new EvalException("Not implemented yet")

}
