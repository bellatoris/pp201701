package pp201701.mid6

/*
 *  Midterm of Principles of Programming 2017
 *  
 *  General Instructions:
 *  - './compile.sh' compiles your code
 *  - './run.sh' runs a simple test case given in Test.scala
 *  - Before submission, check your code by running './run.sh'. Uncompilable code will get 0 points.
 *  - Do not use imperative features. (ex. for, while, var, etc)
 *  - Compress the 'src' folder with '.zip' format for submission.
 */

object Main {
  /*
   Problem 6: Boolean expression

   Define an abstract data type (ADT) for boolean expressions.
   Your ADT should express:
   - True
   - False
   - 5 variables ( A, B, C, D, E )
   - AND expression
   - OR expression
   - NOT expression
   */

  sealed abstract class Bexp
  case class BTrue() extends Bexp
  case class BFalse() extends Bexp
  case class BVarA() extends Bexp
  case class BVarB() extends Bexp
  case class BVarC() extends Bexp
  case class BVarD() extends Bexp
  case class BVarE() extends Bexp
  case class BAnd(e1:Bexp, e2:Bexp) extends Bexp
  case class BOr(e1:Bexp, e2:Bexp) extends Bexp
  case class BNot(e1:Bexp) extends Bexp

  /* Complete Bexp here */

  /*
   Define values and functions for constructing Bexp values.
   */
  val mkTrue : Bexp = BTrue()

  val mkFalse : Bexp = BFalse()

  // If x is not the given variables, make it False.
  def mkVar(x:String) : Bexp =
    x match {
      case "A" => BVarA()
      case "B" => BVarB()
      case "C" => BVarC()
      case "D" => BVarD()
      case "E" => BVarE()
      case _ => BFalse()
    }

  def mkAnd(exp1:Bexp, exp2 :Bexp) : Bexp =
    BAnd(exp1, exp2)

  def mkOr(exp1:Bexp, exp2: Bexp) : Bexp =
    BOr(exp1, exp2)

  def mkNot(exp1:Bexp) : Bexp =
    BNot(exp1)

  /*
   Implement a function that evaluates the given Bexp under the given environment[env].
   The environment assigns values to variables (A, B, C, D, E) during the evaluation.
   */
  def eval(exp:Bexp, env:(Boolean, Boolean, Boolean, Boolean, Boolean)) : Boolean =
    exp match {
      case BTrue() => true
      case BFalse() => false
      case BVarA() => env._1
      case BVarB() => env._2
      case BVarC() => env._3
      case BVarD() => env._4
      case BVarE() => env._5
      case BAnd(e1, e2) => (eval(e1, env) && eval(e2, env))
      case BOr(e1, e2) => (eval(e1, env) || eval(e2, env))
      case BNot(e1) => !(eval(e1, env))
    }
}
