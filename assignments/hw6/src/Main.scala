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

  implicit def realAddProxy: AddOp[Real] = ???

  implicit def realMultProxy: MultOp[Real] = ???

  /***********
   Complex Numbers
   ***********/

  /*
   ComplexNumber class and its companion object
   Use these as constructors of 'Complex' outside 'object Complex',
   instead of using direct constructor 'new Complex(..)'.
   */
  object Complex extends ComplexBasic {
    def makeRectangular(real: Real, imaginary: Real): Complex = ???
    def makePolar(magnitude: Real, angle: Real): Complex = ???
  }

  implicit def complexAddProxy: AddOp[Complex] = ???

  implicit def complexMultProxy: MultOp[Complex] = ???


  /***********
   Polynomials
   ***********/

  /*
   In actual test, we may use types other than complex numbers.
   */
  object Polynomial {
    def eval[A](poly: Polynomial[A], a: A)(implicit multProxy : MultOp[A], addProxy : AddOp[A]) : A = ???
  }

  implicit def polynomialAddProxy[A](implicit multProxy:MultOp[A], addProxy:AddOp[A]): AddOp[Polynomial[A]] = ???

  implicit def polynomialMultProxy[A](implicit multProxy:MultOp[A], addProxy:AddOp[A]): MultOp[Polynomial[A]] = ???

}
