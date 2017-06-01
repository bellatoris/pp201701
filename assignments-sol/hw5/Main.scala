package pp201701.hw5
import pp201701.hw5.Data.DataBundle._

object Main extends App {
  /*
   Implement traits for complex number addition/multiplication.
   */
  trait ComplexNumberAdd extends ComplexNumberSpec {
    def add(that: ComplexNumberSpec): ComplexNumberSpec =
      makeRectangular(real + that.real, imaginary + that.imaginary)
  }

  trait ComplexNumberMult extends ComplexNumberSpec {
    def mult(that: ComplexNumberSpec): ComplexNumberSpec =
      makePolar(magnitude * that.magnitude, ComplexNumberSpec.normalizeAngle(angle + that.angle))
  }

  /*
   Spec of eval
   Input: A polynomial, represented in a list of coefficients.
          For example, (1, 0, i, -1) = 1 + (0 * x) + (i * x^2) + (-1 * x^3)
          Nil means 0
   Output: Calculated result of given polynomial with "this" complex number.
          For example, if (1 + 0*i) calls "eval(1, 0, i, -1)", then output is:
          1 + (0 * (1 + 0*i)) + (i * (1 + 0*i)^2) + (-1 * (1 + 0*i)^3)
          = 1 + 0 + i - 1
          = 0 + i
   */
  trait ComplexNumberEval
      extends ComplexNumberSpec
      with ComplexNumberAdd
      with ComplexNumberMult {
    def eval(coeffs: List[ComplexNumberEval]): ComplexNumberSpec =
      coeffs match {
        case Nil => makePolar(0,0)
        case c::tl => c.add(mult(eval(tl)))
      }
  }

  /*
   Implement each missing fields so that it compiles.
   For constructor,
     if "isRectangular" is true,
       use "arg1" as "real" and "arg2" as "imaginary" in rectangular representation.
     else,
       use "arg1" as "magnitude" and "arg2" as "angle" in polar representation.

   * Note: The "angle" value should always be normalized.

   */
  class ComplexNumberImpl (isRectangular: Boolean, arg1: Double, arg2: Double)
      extends ComplexNumberSpec
      with ComplexNumberAdd
      with ComplexNumberMult
      with ComplexNumberEval {
    // Fill in here.
    val real =
      if (isRectangular) arg1 else ComplexNumberSpec.polarToRectangular(arg1, arg2)._1
    val imaginary =
      if (isRectangular) arg2 else ComplexNumberSpec.polarToRectangular(arg1, arg2)._2

    val magnitude =
      if (isRectangular) ComplexNumberSpec.rectangularToPolar(arg1, arg2)._1 else arg1

    val angle =
      if (isRectangular) ComplexNumberSpec.rectangularToPolar(arg1, arg2)._2 else arg2

    def makeRectangular(real: Double, imaginary: Double): ComplexNumberSpec =
      new ComplexNumberImpl(true, real, imaginary)

    def makePolar(magnitude: Double, angle: Double): ComplexNumberSpec =
      new ComplexNumberImpl(false, magnitude, angle)
  }

}
