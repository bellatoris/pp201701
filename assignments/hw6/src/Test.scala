package pp201701.hw6test
import pp201701.hw6.Main._
import pp201701.hw6.Data.DataBundle._

object Test extends App {
  def print_result(b:Boolean) : Unit =
    if (b) println("O") else println("X")

  val a = Complex.makeRectangular(0, 1)
  print_result(Real.equals(a.magnitude, 1.0))

  val b = Complex.makeRectangular(3, 3)
  val apb = implicitly[AddOp[Complex]].op(a, b)
  print_result(Real.equals(apb.magnitude, 5.0))

  val poly = List[Real](3, 4, 5)
  val poly2 = List[Real](1, 2)
  println(implicitly[AddOp[Polynomial[Real]]].op(poly, poly2))
  println(implicitly[MultOp[Polynomial[Real]]].op(poly, poly2))

  implicit val addOp = new AddOp[Int] {
    def op(a: Int, b: Int): Int = a + b
    val identity: Int = 0
    def inverse(a: Int) = -a
  }

  implicit val multOp = new MultOp[Int] {
    def op(a: Int, b: Int) = a * b
    val identity: Int = 1
  }

  val e1: Polynomial[Int] = List(1, 0, 3, 2)
  val e2: Polynomial[Int] = List(0, 1, 1)
  print_result(Polynomial.eval(e1, 1)(multOp, addOp) == 6)

  val e = implicitly[MultOp[Polynomial[Int]]].op(e1, e2)
  print_result(Polynomial.eval(e, 2)(multOp, addOp) == 174)
}
