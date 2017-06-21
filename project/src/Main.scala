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

  def deleteBind(env: Environment, key: String): Environment = env match {
    case EmptyEnv() => {
      env 
    }
    case NonEmptyEnv(table, prior) => {
      table -= key
      env
    }
  }

  sealed abstract class Val
  case class VInt(n: Int) extends Val
  case class VTrue() extends Val
  case class VFalse() extends Val
  case class VNil() extends Val
  case class VCons(h: Val, t: Val) extends Val
  case class VFun(env: Environment, fname: String, memo: HashMap[List[Val], Val], params: List[String], expr: Expr) extends Val
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
      case VFun(env, fname, memo, params, expr) => true
      case _ => false
    }
  }
}

object Main {
  import Value._

  class EvalException(val msg: String) extends Exception

  // to Value method
  def constToVal(c: Const)(implicit memo: Boolean): Val = c match {
    case CInt(n: Int) => VInt(n)
    case CTrue() => VTrue()
    case CFalse() => VFalse()
    case CNil() => VNil()
  }

  def nameToVal(env: Environment, x: String)(implicit memo: Boolean): Val = env.find(x) match {
    case VExpr(env, expr) => environmentEval(env, expr)
    case VError() => throw new EvalException("There is no bind")
    case _ @ value => value
  }

  def ifToVal(env: Environment, econd: Expr, et: Expr, ef: Expr)(implicit memo: Boolean): Val = 
  environmentEval(env, econd) match {
    case VTrue() => environmentEval(env, et)
    case VFalse() => environmentEval(env, ef)
    case _ => throw new EvalException("Not Boolean Expression")
  }

  def consToVal(env: Environment, eh: Expr, et: Expr)(implicit memo: Boolean): Val = {
    val head = environmentEval(env, eh)
    val tail = environmentEval(env, et)
    tail match {
      case VCons(hd, tl) => VCons(head, tail)
      case VNil() => VCons(head, VNil())
      case _ => VCons(head, VCons(tail, VNil()))
    }
  }

  def headToVal(env: Environment, el: Expr)(implicit memo: Boolean): Val = environmentEval(env, el) match {
    case VCons(hd, tl) => hd
    case VNil() => throw new EvalException("head of empty pair")
    case _ => throw new EvalException("Not pair")
  }

  def tailToVal(env: Environment, el: Expr)(implicit memo: Boolean): Val = environmentEval(env, el) match {
    case VCons(hd, tl) => tl
    case VNil() => throw new EvalException("tail of empty list")
    case _ => throw new EvalException("Not pair")
  }

  def funToVal(env: Environment, params: List[String], eb: Expr)(implicit memo: Boolean): Val = {
    VFun(env, "", HashMap[List[Val], Val](), params, eb)
  }

  def appToVal(env: Environment, ef: Expr, eargs: List[Expr]): Val = { 
    @tailrec
    def tail(env: Environment, ef: Expr, eargs: List[Expr]): (Environment, Expr) = {
      environmentEval(env, ef)(false) match {
        case VFun(funEnv, fname, memo, params, eb) => {
          val args = eargs.map(expr => environmentEval(env, expr)(false))
          val newEnv = funEnv.newEnv
          
          params.zip(args).foreach(arg => addBind(newEnv, arg))

          @tailrec
          def ifLet(env: Environment, expr: Expr): (Environment, Expr) = expr match {
            case EIf(econd, et, ef) => environmentEval(env, econd)(false) match {
              case VTrue() => ifLet(env, et) 
              case VFalse() => ifLet(env, ef)
              case _ => throw new EvalException("Not Boolean Expression")
            }
            case ELet(bs, eb) => {
              val newEnv = env.newEnv
              bs.foreach(bind => bind match {
                  case (BDEF(), str, expr) => expr match {
                    case EFun(params, eb) => addBind(newEnv, (str, VFun(newEnv, str, HashMap[List[Val], Val](), params, eb)))
                    case _ => addBind(newEnv, (str, VExpr(newEnv, expr)))
                  }
                  case (BVAL(), str, expr) => expr match {
                    case EFun(params, eb) => addBind(newEnv, (str, VFun(newEnv, str, HashMap[List[Val], Val](), params, eb)))
                    case _ => addBind(newEnv, (str, environmentEval(newEnv, expr)(false)))
                  }
              })
              ifLet(newEnv, eb)
            }
            case _ => (env, expr)
          }

          val result = ifLet(newEnv, eb) 
          result._2 match {
             case EApp(ef, eargs) => tail(result._1, ef, eargs) 
             case _ => (result._1, eb)
          }
        }
        case _ => throw new EvalException("Not Function")
      }
    }

    val (newEnv, eb) = tail(env, ef, eargs)
    environmentEval(newEnv, eb)(false)
  }

  def appToValMemo(env: Environment, ef: Expr, eargs: List[Expr]): Val = environmentEval(env, ef)(true) match {
    case VFun(funEnv, fname, memo, params, eb) if fname != "" => {
      val args = eargs.map(expr => environmentEval(env, expr)(true))
      memo.get(args) match {
        case Some(v) => v
        case None => {
          val newEnv = funEnv.newEnv

          params.zip(args).foreach(arg => addBind(newEnv, arg))
          val result = environmentEval(newEnv, eb)(true)
          val newMemo = funEnv.find(fname) match {
            case VFun(env, n, m, p, e) => m + (args -> result)
            case _ => throw new EvalException("Never be happened")
          }
          
          deleteBind(funEnv, fname)
          addBind(funEnv, (fname, VFun(funEnv, fname, newMemo, params, eb)))
          result
        }
      }
    }
    case _ => throw new EvalException("Not Function")
  }


  def letToVal(env: Environment, bs: List[Bind], eb: Expr)(implicit memo: Boolean): Val = {
    val newEnv = env.newEnv
   
    bs.foreach(bind => bind match {
        case (BDEF(), str, expr) => expr match {
          case EFun(params, eb) => addBind(newEnv, (str, VFun(newEnv, str, HashMap[List[Val], Val](), params, eb)))
          case _ => addBind(newEnv, (str, VExpr(newEnv, expr)))
        }
        case (BVAL(), str, expr) => expr match {
          case EFun(params, eb) => addBind(newEnv, (str, VFun(newEnv, str, HashMap[List[Val], Val](), params, eb)))
          case _ => addBind(newEnv, (str, environmentEval(newEnv, expr)))
        }
    })

    environmentEval(newEnv, eb)
  }

  def addToVal(env: Environment, e1: Expr, e2: Expr)(implicit memo: Boolean): Val = {
    (environmentEval(env, e1), environmentEval(env, e2)) match {
      case (VInt(n), VInt(m)) => VInt(n + m)
      case _ => throw new EvalException("Not Integer")
    }
  }

  def subToVal(env: Environment, e1: Expr, e2: Expr)(implicit memo: Boolean): Val = {
    (environmentEval(env, e1), environmentEval(env, e2)) match {
      case (VInt(n), VInt(m)) => VInt(n - m)
      case _ => throw new EvalException("Not Integer")
    }
  }

  def multToVal(env: Environment, e1: Expr, e2: Expr)(implicit memo: Boolean): Val = {
    (environmentEval(env, e1), environmentEval(env, e2)) match {
      case (VInt(n), VInt(m)) => VInt(n * m)
      case _ => throw new EvalException("Not Integer")
    }
  }

  def equalToVal(env: Environment, e1: Expr, e2: Expr)(implicit memo: Boolean): Val = {
    (environmentEval(env, e1), environmentEval(env, e2)) match {
      case (VInt(n), VInt(m)) => if (n == m) VTrue() else VFalse()
      case _ => throw new EvalException("Not Integer")
    }
  }

  def lessToVal(env: Environment, e1: Expr, e2: Expr)(implicit memo: Boolean): Val = {
    (environmentEval(env, e1), environmentEval(env, e2)) match {
      case (VInt(n), VInt(m)) => if (n < m) VTrue() else VFalse()
      case _ => throw new EvalException("Not Integer")
    }
  }

  def greaterToVal(env: Environment, e1: Expr, e2: Expr)(implicit memo: Boolean): Val = {
    (environmentEval(env, e1), environmentEval(env, e2)) match {
      case (VInt(n), VInt(m)) => if (n > m) VTrue() else VFalse()
      case _ => throw new EvalException("Not Integer")
    }
  }

  def myeval(e:Expr) : Val =  {
    environmentEval(EmptyEnv(), e)(false)
  }

  def environmentEval(env: Environment, e: Expr)(implicit memo: Boolean): Val = e match {
    case EConst(c) => constToVal(c)
    case EName(x) => nameToVal(env, x)
    case EIf(econd, et, ef) => ifToVal(env, econd, et, ef)
    case ECons(eh, et) => consToVal(env,eh, et)
    case EHd(el) => headToVal(env, el)
    case ETl(el) => tailToVal(env, el)
    case EFun(params, eb) => funToVal(env, params, eb)
    case EApp(ef, eargs) => if (memo) appToValMemo(env, ef, eargs) else appToVal(env, ef, eargs)
    case ELet(bs, eb) => letToVal(env, bs, eb)
    case EPlus(e1, e2) => addToVal(env, e1, e2)
    case EMinus(e1, e2) => subToVal(env, e1, e2)
    case EMult(e1, e2) => multToVal(env, e1, e2)
    case EEq(e1, e2) => equalToVal(env, e1, e2)
    case ELt(e1, e2) => lessToVal(env, e1, e2)
    case EGt(e1, e2) => greaterToVal(env, e1, e2)
  }

  def myeval_memo(e:Expr) : Val = {
    environmentEval(EmptyEnv(), e)(true)
  }

}
