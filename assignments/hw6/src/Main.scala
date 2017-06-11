package pp201701.hw6
import pp201701.hw6.Data.DataBundle._
import scala.math._

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
  /*
   HW6: Define Typeclasses for Real number, Complex number, and Polynomials.
   */

  /***********
   Real Numbers
   ***********/

  implicit def realAddProxy: AddOp[Real] = new AddOp[Real] {
    def op(a: Real, b: Real): Real = a + b
    val identity: Real = 0.0
    def inverse(a: Real): Real = -a
  }

  implicit def realMultProxy: MultOp[Real] = new MultOp[Real] {
    def op(a: Real, b: Real): Real = a * b
    val identity: Real = 1.0
  }

  /***********
   Complex Numbers
   ***********/

  /*
   ComplexNumber class and its companion object
   Use these as constructors of 'Complex' outside 'object Complex',
   instead of using direct constructor 'new Complex(..)'.
   */
  object Complex extends ComplexBasic {
    def makeRectangular(real: Real, imaginary: Real): Complex = {
      val (magnitude, angle) = ComplexPrivate.rectangularToPolar(real, imaginary)
      new Complex(real, imaginary, magnitude, ComplexPrivate.normalizeAngle(angle))
    }
    def makePolar(magnitude: Real, angle: Real): Complex = {
      val (real, imaginary) = ComplexPrivate.polarToRectangular(magnitude, angle)
      new Complex(real, imaginary, magnitude, ComplexPrivate.normalizeAngle(angle))
    }
  }

  implicit def complexAddProxy: AddOp[Complex] = new AddOp[Complex] {
    def op(a: Complex, b: Complex): Complex = 
      Complex.makeRectangular(a.real + b.real, a.imaginary + b.imaginary)
    val identity: Complex = Complex.makeRectangular(0, 0)
    def inverse(a: Complex): Complex = Complex.makeRectangular(-a.real, -a.imaginary)
  }

  implicit def complexMultProxy: MultOp[Complex] = new MultOp[Complex] {
    def op(a: Complex, b: Complex): Complex = 
      Complex.makeRectangular(a.real * b.real - a.imaginary * b.imaginary,
                              a.real * b.imaginary + a.imaginary * b.real)
    val identity: Complex = Complex.makeRectangular(1, 0)
  }


  /***********
   Polynomials
   ***********/

  /*
   In actual test, we may use types other than complex numbers.
   */
  object Polynomial {
    def eval[A](poly: Polynomial[A], a: A)(implicit multProxy : MultOp[A], addProxy : AddOp[A]) : A = poly match {
      case Nil => addProxy.identity
      case c :: tl => addProxy.op(c, multProxy.op(a, eval(tl, a)(multProxy, addProxy)))
    }
  }

  implicit def polynomialAddProxy[A](implicit multProxy:MultOp[A], addProxy:AddOp[A]): AddOp[Polynomial[A]] = new AddOp[Polynomial[A]] {
    def op(a: Polynomial[A], b: Polynomial[A]): Polynomial[A] = (a, b) match {
      case (Nil, _) => b
      case (_, Nil) => a
      case (_, _) => a.zipAll(b, addProxy.identity, addProxy.identity).map(pair => addProxy.op(pair._1, pair._2))
    }
    val identity: Polynomial[A] = List(addProxy.identity)
    def inverse(a: Polynomial[A]) = a match {
      case Nil => a
      case _ => a.map(coeff => addProxy.inverse(coeff))
    }
  }

  implicit def polynomialMultProxy[A](implicit multProxy:MultOp[A], addProxy:AddOp[A]): MultOp[Polynomial[A]] = new MultOp[Polynomial[A]] {
    def op(a: Polynomial[A], b: Polynomial[A]): Polynomial[A] = (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (_, _) => {
        def mul(poly: Polynomial[A], coeff: A, power: Int): Polynomial[A] = {
          def makeEmptyPolynomial(power: Int): Polynomial[A] = power match {
            case 0 => Nil
            case _ => addProxy.identity :: makeEmptyPolynomial(power - 1)
          }
          makeEmptyPolynomial(power) ++ poly.map(x => multProxy.op(x, coeff))
        }
        a.zipWithIndex.map(coeffAndPower => mul(b, coeffAndPower._1, coeffAndPower._2)).foldLeft(List(addProxy.identity))((sum, poly) => polynomialAddProxy[A].op(sum, poly))
      }
    }
    val identity: Polynomial[A] = List(multProxy.identity)
  }

}
