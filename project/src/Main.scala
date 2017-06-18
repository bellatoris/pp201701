package pp201701.proj
import scala.annotation.tailrec
import pp201701.proj.Data.DataBundle._
import scala.collection.immutable._

object Value {
  type Table = scala.collection.mutable.HashMap[String, Val]
  
  // Environment
  sealed abstract class Environment {
    def find(x: String): Val
    def newEnv: Environment = NonEmptyEnv(scala.collection.mutable.HashMap[String, Val](), this)
  }
  case class EmptyEnv() extends Environment {
    def find(x: String) = VError()
  }
  case class NonEmptyEnv(table: Table, 
                         priorEnvironment: Environment) extends Environment {
    def find(x: String) = table.get(x) match {
      case Some(value) => value
      case None => priorEnvironment.find(x)
    }
  }

  def addBind(env: Environment, bind: (String, Val)): Environment = env match {
    case EmptyEnv() => {
      // cannot add bind to EmptyEnv
      env 
    }
    case NonEmptyEnv(table, prior) => {
      table += (bind._1 -> bind._2)
      env
    }
  }

  sealed abstract class Val
  case class VInt(n: Int) extends Val
  case class VTrue() extends Val
  case class VFalse() extends Val
  case class VNil() extends Val
  case class VCons(h: Val, t: Val) extends Val
  case class VFun(env: Environment, params: List[String], expr: Expr) extends Val
  case class VError() extends Val
  case class VExpr(env: Environment, expr: Expr) extends Val

  abstract class ConvertToScala[A] {
    def toInt(a:A) : Option[Int]
    def toBool(a:A) : Option[Boolean]
    def toPair(a:A) : Option[(A, A)]
    def isNil(a:A) : Boolean
    def isFun(a:A) : Boolean
  }

  implicit val valConv : ConvertToScala[Val] = new ConvertToScala[Val] {
    def toInt(a: Val): Option[Int] = a match {
      case VInt(n) => Some(n)
      case _ => None
    }
    def toBool(a: Val): Option[Boolean] = a match {
      case VFalse() => Some(false)
      case VTrue() => Some(true)
      case _ => None
    }
    def toPair(a: Val): Option[(Val, Val)] = a match {
      case VCons(h, t) => Some((h, t))
      case _ => None
    }
    def isNil(a: Val): Boolean = a match {
      case VNil() => true
      case _ => false
    }
    def isFun(a: Val): Boolean = a match {
      case VFun(env, params, expr) => true
      case _ => false
    }
  }
}

object Main {
  import Value._

  class EvalException(val msg: String) extends Exception

  // to Value method
  def constToVal(c: Const): Val = c match {
    case CInt(n: Int) => VInt(n)
    case CTrue() => VTrue()
    case CFalse() => VFalse()
    case CNil() => VNil()
  }

  def nameToVal(env: Environment, x: String): Val = env.find(x) match {
    case VExpr(env, expr) => environmentEval(env, expr)
    case VError() => throw new EvalException("There is no bind")
    case _ @ value => value
  }

  def ifToVal(env: Environment, econd: Expr, et: Expr, ef: Expr): Val = 
  environmentEval(env, econd) match {
    case VTrue() => environmentEval(env, et)
    case VFalse() => environmentEval(env, ef)
    case _ => throw new EvalException("Not Boolean Expression")
  }

  def consToVal(env: Environment, eh: Expr, et: Expr): Val = {
    val head = environmentEval(env, eh)
    val tail = environmentEval(env, et)
    tail match {
      case VCons(hd, tl) => VCons(head, tail)
      case VNil() => VCons(head, VNil())
      case _ => VCons(head, VCons(tail, VNil()))
    }
  }

  def headToVal(env: Environment, el: Expr): Val = environmentEval(env, el) match {
    case VCons(hd, tl) => hd
    case VNil() => throw new EvalException("head of empty pair")
    case _ => throw new EvalException("Not pair")
  }

  def tailToVal(env: Environment, el: Expr): Val = environmentEval(env, el) match {
    case VCons(hd, tl) => tl
    case VNil() => throw new EvalException("tail of empty list")
    case _ => throw new EvalException("Not pair")
  }

  def funToVal(env: Environment, params: List[String], eb: Expr): Val = {
    VFun(env, params, eb)
  }

  def appToVal(env: Environment, ef: Expr, eargs: List[Expr]): Val = 
  environmentEval(env, ef) match {
    case VFun(funEnv, params, eb) => {
      if (params.length != eargs.length) {
        throw new EvalException("Arguments' length does not match with parameter")
      } else {
        val args = eargs.map(expr => environmentEval(env, expr))
        val newEnv = funEnv.newEnv
        
        params.zip(args).foreach(arg => addBind(newEnv, arg))
        environmentEval(newEnv, eb)
      }
    }
    case _ => throw new EvalException("Not Function")
  }

  def letToVal(env: Environment, bs: List[Bind], eb: Expr): Val = {
    val newEnv = env.newEnv
   
    bs.foreach(bind => bind match {
        case (BDEF(), str, expr) => addBind(newEnv, (str, VExpr(newEnv, expr)))
        case (BVAL(), str, expr) => addBind(newEnv, (str, environmentEval(newEnv, expr)))
    })

    environmentEval(newEnv, eb)
  }

  def addToVal(env: Environment, e1: Expr, e2: Expr): Val = {
    (environmentEval(env, e1), environmentEval(env, e2)) match {
      case (VInt(n), VInt(m)) => VInt(n + m)
      case _ => throw new EvalException("Not Integer")
    }
  }

  def subToVal(env: Environment, e1: Expr, e2: Expr): Val = {
    (environmentEval(env, e1), environmentEval(env, e2)) match {
      case (VInt(n), VInt(m)) => VInt(n - m)
      case _ => throw new EvalException("Not Integer")
    }
  }

  def multToVal(env: Environment, e1: Expr, e2: Expr): Val = {
    (environmentEval(env, e1), environmentEval(env, e2)) match {
      case (VInt(n), VInt(m)) => VInt(n * m)
      case _ => throw new EvalException("Not Integer")
    }
  }

  def equalToVal(env: Environment, e1: Expr, e2: Expr): Val = {
    (environmentEval(env, e1), environmentEval(env, e2)) match {
      case (VInt(n), VInt(m)) => if (n == m) VTrue() else VFalse()
      case _ => throw new EvalException("Not Integer")
    }
  }

  def lessToVal(env: Environment, e1: Expr, e2: Expr): Val = {
    (environmentEval(env, e1), environmentEval(env, e2)) match {
      case (VInt(n), VInt(m)) => if (n < m) VTrue() else VFalse()
      case _ => throw new EvalException("Not Integer")
    }
  }

  def greaterToVal(env: Environment, e1: Expr, e2: Expr): Val = {
    (environmentEval(env, e1), environmentEval(env, e2)) match {
      case (VInt(n), VInt(m)) => if (n > m) VTrue() else VFalse()
      case _ => throw new EvalException("Not Integer")
    }
  }

  def myeval(e:Expr) : Val =  {
    environmentEval(EmptyEnv(), e)
  }

  @tailrec
  def environmentEval(env: Environment, e: Expr): Val = e match {
    case EConst(c) => constToVal(c)
    case EName(x) => nameToVal(env, x)
    case EIf(econd, et, ef) => ifToVal(env, econd, et, ef)
    case ECons(eh, et) => consToVal(env,eh, et)
    case EHd(el) => headToVal(env, el)
    case ETl(el) => tailToVal(env, el)
    case EFun(params, eb) => funToVal(env, params, eb)
    case EApp(ef, eargs) => appToVal(env, ef, eargs)
    case ELet(bs, eb) => letToVal(env, bs, eb)
    case EPlus(e1, e2) => addToVal(env, e1, e2)
    case EMinus(e1, e2) => subToVal(env, e1, e2)
    case EMult(e1, e2) => multToVal(env, e1, e2)
    case EEq(e1, e2) => equalToVal(env, e1, e2)
    case ELt(e1, e2) => lessToVal(env, e1, e2)
    case EGt(e1, e2) => greaterToVal(env, e1, e2)
  }

  def myeval_memo(e:Expr) : Val = throw new EvalException("Not implemented yet")

}
