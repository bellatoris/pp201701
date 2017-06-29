package pp201701.fin
import pp201701.fin.Data.DataBundle._
import pp201701.fin.Data.BoxObj._

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
   Problem 6: Define type classes of Dict and Report for OrdDictionary.
   OrdDictionary is a wrapper of D, which comes with Dict[D, K, V] but its implemention is unknown.
   The iteration order of OrdDictionary should follow the order of insertion.

   Assume that we can compare K's elements with "=".

   Hint: Use 'keys' in order to store the order of insertion.
   */

  class OrdDictionary[D, K](val d:D, val keys : List[K])

  implicit def DictOrdDictionary[D, K, V](implicit DictD : Dict[D, K, V]) :
      Dict[OrdDictionary[D, K], K, V] =
    new Dict[OrdDictionary[D, K], K, V] {
      def empty = new OrdDictionary(DictD.empty, Nil)
      def add(o:OrdDictionary[D, K], k:K, v:V) =
        new OrdDictionary(DictD.add(o.d, k, v), o.keys.filter((a)=>a != k) ++ List(k))
      def lookup(o:OrdDictionary[D, K], k:K) =
        DictD.lookup(o.d, k)
    }

  def OrdDictIter[D, K, V] (DictD : Dict[D, K, V]) : Iter[(D, List[K]), (K, V)] =
    new Iter[(D, List[K]), (K, V)]{
      def getValue(i:(D, List[K])):Option[(K, V)] =
        i._2 match {
          case Nil => None
          case h::t => DictD.lookup(i._1, h) match {
            case None => None
            case Some(v) => Some(h, v)
          }
        }
      def getNext(i:(D, List[K])): (D, List[K]) =
        i._2 match {
          case Nil => i
          case h::t => (i._1, t)
        }
    }

  def ReportOrdDictionary[D, K, V](ord_title : String, keyToString:(K)=>String,
    valToString:(V)=>String)(implicit DictD: Dict[D, K, V]) :
      Report[OrdDictionary[D, K]] =
    new Report[OrdDictionary[D, K]] {
      type A = (K, V)
      def title(r:OrdDictionary[D, K]) = ord_title
      def it : Iterable[OrdDictionary[D, K], A] =
        new Iterable[OrdDictionary[D, K], A] {
          def iter (a:OrdDictionary[D, K]) : Box2[Iter, A] = {
            Box2((a.d, a.keys))(OrdDictIter(DictD))
          }
        }
      def keyval : KeyVal[(K, V)] = new KeyVal[(K, V)] {
        def get_key(a:(K, V)) = keyToString(a._1)
        def get_val(a:(K, V)) = valToString(a._2)
      }
    }

}
