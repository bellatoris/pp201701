package pp201701.hw5
import pp201701.hw5.Data.DataBundle._

object Main extends App {
  /*
   Implement traits for complex number addition/multiplication.
   */
  trait ComplexNumberAdd extends ComplexNumberSpec {
    def add(that: ComplexNumberSpec): ComplexNumberSpec = {
      makeRectangular(real + that.real, imaginary + that.imaginary)
    }
  }

  trait ComplexNumberMult extends ComplexNumberSpec {
    def mult(that: ComplexNumberSpec): ComplexNumberSpec = {
      makeRectangular(real * that.real - imaginary * that.imaginary, real * that.imaginary + that.real * imaginary)
    }
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
    def eval(coeffs: List[ComplexNumberEval]): ComplexNumberSpec = {
      def pow(degree: Int): ComplexNumberSpec = degree match {
        case 0 => makeRectangular(1, 0)
        case _ => mult(pow(degree - 1))
      }
      
      val polynomial = coeffs.zipWithIndex.map(pair => pair._1.mult(pow(pair._2)))
      val (real, imaginary) = polynomial.foldLeft((0.0, 0.0))((sum, complex) => (sum._1 + complex.real, sum._2 + complex.imaginary))
      makeRectangular(real, imaginary) 
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
    override val (real, imaginary) = {
      if (isRectangular) {
        (arg1, arg2) 
      } else { 
        ComplexNumberSpec.polarToRectangular(arg1, 
          ComplexNumberSpec.normalizeAngle(arg2))
      }
    }
    override val (magnitude, angle) = {
      if (!isRectangular) {
        (arg1, ComplexNumberSpec.normalizeAngle(arg2))
      } else { 
        val pair = ComplexNumberSpec.rectangularToPolar(arg1, arg2)
        (pair._1, ComplexNumberSpec.normalizeAngle(pair._2)) 
      }
    }

    override def makeRectangular(real: Double, imaginary: Double) = {
      new ComplexNumberImpl(true, real, imaginary)
    }

    override def makePolar(magnitude: Double, angle: Double) = {
      new ComplexNumberImpl(false, magnitude, angle)
    }
  }
}
