package pp201701.proj
import scala.annotation.tailrec
import pp201701.proj.Data.DataBundle._
import scala.collection.immutable._

object Value {
  // Environment

  sealed abstract class EnvVal
  case class EDef(e:Expr) extends EnvVal
  case class EVal(v:Val) extends EnvVal

  type PEnv = List[(String, EnvVal)]
  type Env = List[PEnv]
  val emptyenv : Env = List(Nil)

  // Value
  sealed abstract class Val
  case class VInt(n:Int) extends Val
  case class VBool(b:Boolean) extends Val
  case class VNil() extends Val
  case class VCons(hd:Val, tl:Val) extends Val
  case class VFun(params:List[String], body:Expr, env:Env) extends Val

  def val_toString(v:Val):String =
    v match {
      case VInt(n) => n.toString
      case VBool(b) => b.toString
      case VNil() => "nil"
      case VCons(hd, tl) => "(cons " + val_toString(hd) + ", " + val_toString(tl) + ")"
      case VFun(p, b, env) => "fun"
    }

  abstract class ConvertToScala[A] {
    def toInt(a:A) : Option[Int]
    def toBool(a:A) : Option[Boolean]
    def toPair(a:A) : Option[(A, A)]
    def isNil(a:A) : Boolean
    def isFun(a:A) : Boolean
  }

  implicit val valConv : ConvertToScala[Val] = new ConvertToScala[Val] {
    def toInt(v:Val) = v match {
      case VInt(n) => Some(n)
      case _ => None
    }
    def toBool(v:Val) = v match {
      case VBool(b) => Some(b)
      case _ => None
    }
    def toPair(v:Val) = v match {
      case VCons(h, t) => Some(h, t)
      case _ => None
    }
    def isNil(v:Val) = v match {
      case VNil() => true
      case _ => false
    }
    def isFun(v:Val) = v match {
      case VFun(_, _, _) => true
      case _ => false
    }
  }
}

object Main {
  import Value._

  class EvalException(val msg: String) extends Exception

  object Eval {
    def find_penv(pe:PEnv, x:String): Option[EnvVal] =
      pe.find(a => a._1 == x) match {
        case Some((_, ev)) => Some(ev)
        case None => None
      }

    def find_env(env:Env, x:String): Val =
      env match {
        case Nil => throw new EvalException(x + " not found in the current environment!")
        case pe::parent_env =>
          find_penv(pe, x) match {
            case None => find_env(parent_env, x)
            case Some(EVal(v)) => v
            case Some(EDef(e)) => myeval_env(e, env)
          }
      }

    def myeval_c(c:Const) : Val =
      c match {
        case CInt(n) => VInt(n)
        case CTrue() => VBool(true)
        case CFalse() => VBool(false)
        case CNil() => VNil()
      }

    def make_let_pe (bs:List[Bind], env:Env) : PEnv = {
      def go(acc:PEnv, bs:List[Bind], env:Env) : PEnv =
        bs match {
          case Nil => acc
          case (k, x, e)::bs_t => k match {
            case BDEF() => go((x, EDef(e))::acc, bs_t, env)
            case BVAL() => go((x, EVal(myeval_env(e, acc::env)))::acc, bs_t, env)
          }
        }
      go(Nil, bs, env)
    }

    // tailrec
    def myeval_env2(e:Expr, env:Env) : Val =
      myeval_env(e, env)

    @tailrec
    def myeval_env(e:Expr, env:Env) : Val =
      e match {
        case EConst(c) => myeval_c(c)
        case EName(x) => find_env(env, x)
        case EIf(ec, et, ef) =>
          (myeval_env2(ec, env)) match {
            case VBool(b) =>
              if (b) myeval_env(et, env) else myeval_env(ef, env)
            case _ => throw new EvalException("Condition of If is not boolean!")
          }
        case ECons(e1, e2) =>
          VCons(myeval_env2(e1, env), myeval_env2(e2, env))
        case EHd(el) =>
          myeval_env2(el, env) match {
            case VCons(hd, tl) => hd
            case _ => throw new EvalException("Tried to get head of illegal values")
          }
        case ETl(el) =>
          myeval_env2(el, env) match {
            case VCons(hd, tl) => tl
            case _ => throw new EvalException("Tried to get tail of illegal values")
          }
        case EFun(p, eb) => VFun(p, eb, env)
        case EApp(ef, eal) => {
          val al:List[EnvVal] = eal.map((e) => EVal(myeval_env2(e, env)))
          myeval_env2(ef, env) match {
            case VFun(ps, eb, fenv) => {
              if (ps.length != al.length) throw new EvalException("Function application arity mismatch")
              else myeval_env(eb, ps.zip(al)::fenv)
            }
            case _ => throw new EvalException("Function application with non-function value")
          }
        }

        case ELet(bs, eb) => {
          val let_pe : PEnv = make_let_pe(bs, env)
          myeval_env(eb, let_pe::env)
        }

        case EPlus(e1, e2) =>
          (myeval_env2(e1, env), myeval_env2(e2, env)) match {
            case (VInt(n1), VInt(n2)) => VInt(n1+n2)
            case _ => throw new EvalException("Addition of non-integers")
          }
        case EMinus(e1, e2) =>
          (myeval_env2(e1, env), myeval_env2(e2, env)) match {
            case (VInt(n1), VInt(n2)) => VInt(n1-n2)
            case _ => throw new EvalException("Subtraction of non-integers")
          }
        case EMult(e1, e2) =>
          (myeval_env2(e1, env), myeval_env2(e2, env)) match {
            case (VInt(n1), VInt(n2)) => VInt(n1*n2)
            case _ => throw new EvalException("Multiplication of non-integers")
          }
        case EEq(e1, e2) =>
          (myeval_env2(e1, env), myeval_env2(e2, env)) match {
            case (VInt(n1), VInt(n2)) => VBool(n1 == n2)
            case (VBool(b1), VBool(b2)) => VBool(b1 == b2)
            case _ => throw new EvalException("Equality of non-primitive values")
          }
        case ELt(e1, e2) =>
          (myeval_env2(e1, env), myeval_env2(e2, env)) match {
            case (VInt(n1), VInt(n2)) => VBool(n1<n2)
            case _ => throw new EvalException("Less-than operation of non-integers ")
          }
        case EGt(e1, e2) =>
          (myeval_env2(e1, env), myeval_env2(e2, env)) match {
            case (VInt(n1), VInt(n2)) => VBool(n1>n2)
            case _ => throw new EvalException("Greater-than operation of non-integers ")
          }
      }
  }

  // memo

  object EvalMemo {
    type MemoTy = HashMap[(Val, List[EnvVal]), Val]
    val empty_map:MemoTy = HashMap.empty[(Val, List[EnvVal]), Val]

    def find_penv(pe:PEnv, x:String): Option[EnvVal] =
      pe.find(a => a._1 == x) match {
        case Some((_, ev)) => Some(ev)
        case None => None
      }

    def find_env(env:Env, x:String, map:MemoTy): (Val, MemoTy) =
      env match {
        case Nil => throw new EvalException(x + " not found in the current environment!")
        case pe::parent_env =>
          find_penv(pe, x) match {
            case None => find_env(parent_env, x, map)
            case Some(EVal(v)) => (v, map)
            case Some(EDef(e)) => myeval_env(e, env, map)
          }
      }

    def myeval_c(c:Const) : Val =
      c match {
        case CInt(n) => VInt(n)
        case CTrue() => VBool(true)
        case CFalse() => VBool(false)
        case CNil() => VNil()
      }

    def make_let_pe (bs:List[Bind], env:Env, map:MemoTy) : (PEnv, MemoTy) = {
      def go(acc:PEnv, bs:List[Bind], env:Env, map:MemoTy) : (PEnv, MemoTy) =
        bs match {
          case Nil => (acc, map)
          case (k, x, e)::bs_t => k match {
            case BDEF() => go((x, EDef(e))::acc, bs_t, env, map)
            case BVAL() => {
              val (v1, map1) = myeval_env(e, acc::env, map)
              go((x, EVal(v1))::acc, bs_t, env, map1)
            }
          }
        }
      go(Nil, bs, env, map)
    }

    // tailrec
    // def myeval_env2(e:Expr, env:Env) : Val =
    //   myeval_env(e, env)

    // @tailrec
    def myeval_env(e:Expr, env:Env, map:MemoTy) : (Val, MemoTy) =
      e match {
        case EConst(c) => (myeval_c(c), map)
        case EName(x) => find_env(env, x, map)
        case EIf(ec, et, ef) =>
          (myeval_env(ec, env, map)) match {
            case (VBool(b), map1) =>
              if (b) myeval_env(et, env, map1) else myeval_env(ef, env, map1)
            case _ => throw new EvalException("Condition of If is not boolean!")
          }
        case ECons(e1, e2) => {
          val (v1, map1) = myeval_env(e1, env, map)
          val (v2, map2) = myeval_env(e2, env, map1)
          (VCons(v1, v2), map2)
        }
        case EHd(el) =>
          myeval_env(el, env, map) match {
            case (VCons(hd, tl), map1) => (hd, map1)
            case _ => throw new EvalException("Tried to get head of illegal values")
          }
        case ETl(el) =>
          myeval_env(el, env, map) match {
            case (VCons(hd, tl), map1) => (tl, map1)
            case _ => throw new EvalException("Tried to get tail of illegal values")
          }
        case EFun(p, eb) => (VFun(p, eb, env), map)
        case EApp(ef, eal) => {
          val (v, map1) = myeval_env(ef, env, map)
          val (al:List[EnvVal], map2:MemoTy) = eal.foldLeft((List.empty[EnvVal], map1))( (lm:(List[EnvVal], MemoTy), e:Expr) => {
            val (v:Val, m1:MemoTy) = myeval_env(e, env, lm._2)
            (lm._1 :+ EVal(v), m1)
          })

          if (map2 contains ((v, al))) (map2((v, al)), map2)
          else
            v match {
              case VFun(ps, eb, fenv) => {
                if (ps.length != al.length) throw new EvalException("Function application arity mismatch")
                else {
                  val (ret, map3) = myeval_env(eb, ps.zip(al)::fenv, map2)
                  (ret, map3 + ((v, al) -> ret))
                }
              }
              case _ => throw new EvalException("Function application with non-function value")
            }
        }

        case ELet(bs, eb) => {
          val (let_pe : PEnv, map1:MemoTy) = make_let_pe(bs, env, map)
          myeval_env(eb, let_pe::env, map1)
        }

        case EPlus(e1, e2) => {
          val (v1, map1) = myeval_env(e1, env, map)
          val (v2, map2) = myeval_env(e2, env, map1)

          (v1, v2) match {
            case (VInt(n1), VInt(n2)) => (VInt(n1+n2), map2)
            case _ => throw new EvalException("Addition of non-integers")
          }
        }
        case EMinus(e1, e2) => {
          val (v1, map1) = myeval_env(e1, env, map)
          val (v2, map2) = myeval_env(e2, env, map1)

          (v1, v2) match {
            case (VInt(n1), VInt(n2)) => (VInt(n1-n2), map2)
            case _ => throw new EvalException("Subtraction of non-integers")
          }
        }
        case EMult(e1, e2) => {
          val (v1, map1) = myeval_env(e1, env, map)
          val (v2, map2) = myeval_env(e2, env, map1)

          (v1, v2) match {
            case (VInt(n1), VInt(n2)) => (VInt(n1*n2), map2)
            case _ => throw new EvalException("Multiplication of non-integers")
          }
        }
        case EEq(e1, e2) => {
          val (v1, map1) = myeval_env(e1, env, map)
          val (v2, map2) = myeval_env(e2, env, map1)

          (v1, v2) match {
            case (VInt(n1), VInt(n2)) => (VBool(v1 == v2), map2)
            case _ => throw new EvalException("Equality of non-integers")
          }
        }
        case ELt(e1, e2) => {
          val (v1, map1) = myeval_env(e1, env, map)
          val (v2, map2) = myeval_env(e2, env, map1)

          (v1, v2) match {
            case (VInt(n1), VInt(n2)) => (VBool(n1<n2), map2)
            case _ => throw new EvalException("Less-than of non-integers")
          }
        }
        case EGt(e1, e2) => {
          val (v1, map1) = myeval_env(e1, env, map)
          val (v2, map2) = myeval_env(e2, env, map1)

          (v1, v2) match {
            case (VInt(n1), VInt(n2)) => (VBool(n1>n2), map2)
            case _ => throw new EvalException("Greater-than of non-integers")
          }
        }
      }
  }

  def myeval(e:Expr) : Val = Eval.myeval_env(e, emptyenv)

  def myeval_memo(e:Expr) : Val = EvalMemo.myeval_env(e, emptyenv, EvalMemo.empty_map)._1

}
