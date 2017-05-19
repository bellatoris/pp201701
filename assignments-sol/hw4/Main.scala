package pp201701.hw4
import pp201701.hw4.Data.DataBundle._

/*
 * ** The submitted code should be runnable. Before upload, you MUST check it
      whether Test.scala runs successfully. Otherwise you may get 0 points for
      all problems.

 * ** Compress the 'src' folder only. Don't put .sh files in the zip file.
      You can put the .iml file together, if you feel difficulty for some reasons.

 * ** Use only the features taught in class. Especially, don't use imperative
      features such as while, for, return, and so on. You may get deduction
      in your points.

 * ** Do not use equivalent built-in features in Scala. The core logic should be
      implemented by you.

 * ** When you give up a problem, use the commented code:
      ex.) If you want to give up the first problem,
        def push[A](stk: Stack[A])(a: A): Stack[A] = stk
 */

object Main {
  /* Problem 1
   Complete an implementation of 'IterDict'.

   IterDict represents a dictionary type.
   (See https://en.wikipedia.org/wiki/Associative_array for more information.)

   Briefly, you can store (key, value) pair into a dictionary,
   and search the stored value later from the associated key value.

   Here, your dictionary should be properly iterable.
   */

  object Problem1 {
    object IterDictImpl {
      //Write empty IterDict
      def empty[K, V](eqFunc: K => K => Boolean): IterDict[K, V] =
        new IterDictImpl(eqFunc)(Nil)
    }

    class IterDictImpl[K, V](eqFunc: K => K => Boolean)(val data: List[(K, V)])
        extends IterDict[K, V] {

      def getValue =
        data match {
          case Nil => None
          case h::t => Some(h)
        }

      def getNext =
        data match {
          case Nil => IterDictImpl.empty[K, V](eqFunc)
          case h::t => new IterDictImpl[K, V](eqFunc)(t)
        }

      // When the given key already exists in this dictionary, overwrite the value.
      def add(k: K, v: V) : IterDict[K, V] = {
        def go(l:List[(K, V)]):List[(K, V)] =
          l match {
            case Nil => (k, v)::Nil
            case h::t => if (h._1 == k) (k, v)::t else h::go(t)
          }
        new IterDictImpl(eqFunc)(go(data))
      }

      // Return the associated value with the key. When there is no such key, return None.
      def find(k: K) : Option[V] = {
        def _find(i:Iter[(K, V)]):Option[V] =
          i.getValue match {
            case None => None
            case Some(h) => if (h._1 == k) Some(h._2) else _find(i.getNext)
          }
        _find(this)
      }
    }

    // Write a function that iterates through given iterator and sum it.
    def sumElements[K](xs: Iter[(K, Int)]): Int =
      xs.getValue match {
        case None => 0
        case Some(h) => h._2 + sumElements(xs.getNext)
      }
  }

  // Problem 2
  object Problem2 {
    /*
     Define 'BiIterable' list and tree classes.
     These should provide bi-directional iteration,
     so you can freely move the iterator forward(getNext) or backward(getPrev)
     at any moment.

     At each end, your BiIter cannot go further, but should be able to go back
     to the reversed direction.

     For example:
     When you are on the first element,
     - getPrev.getValue gives None.
     - getPrev.getPrev.getValue also gives None.
     - getPrev.getPrev.getPrev.getPrev.getNext.getValue gives Some(first-element)

     Similarly, when you are on the last element,
     - getNext.getValue gives None.
     - getNext.getNext.getValue also gives None.
     - getNext.getNext.getNext.getNext.getPrev.getValue gives Some(last-element)

     You may create your own classes for this exercise.
     */
    class MyBiIterImpl[A](val prev:List[A], val cur:Option[A], val next:List[A]) extends BiIter[A] {
      def getValue = cur
      def getNext: BiIter[A] = {
        val prev_n =
          cur match {
            case None => prev
            case Some(c) => c::prev
          }
        next match {
          case Nil => new MyBiIterImpl[A](prev_n, None, Nil)
          case h::t => new MyBiIterImpl[A](prev_n, Some (h), t)
        }
      }

      def getPrev: BiIter[A] = {
        val next_n =
          cur match {
            case None => next
            case Some(c) => c::next
          }
        prev match {
          case Nil => new MyBiIterImpl[A](Nil, None, next_n)
          case h::t => new MyBiIterImpl[A](t, Some (h), next_n)
        }
      }
    }
    object MyBiIterImpl {
      // works only when l and r are on the first element
      def toList[A](i:MyBiIterImpl[A]):List[A] = {
        val cur_l =
          i.cur match {
            case None => Nil
            case Some(a) => a::Nil
          }
        i.prev ++ cur_l ++ i.next
      }

      // def merge[A](l:MyBiIterImpl[A], r:MyBiIterImpl[A]): MyBiIterImpl[A] =
      //   (toList(l) ++ toList(r)) match {
      //     case Nil => new MyBiIterImpl(Nil, None, Nil)
      //     case h::t => new MyBiIterImpl(Nil, Some(h), t)
      //   }
    }

    class BiIterableList[A](val data: List[A]) extends BiIterable[A] {
      def biIter: BiIter[A] =
        data match {
          case Nil => new MyBiIterImpl(Nil, None, Nil)
          case h::t => new MyBiIterImpl(Nil, Some(h), t)
        }
    }

    sealed abstract class BiIterableTree[A] extends BiIterable[A] {
      // You can write something here
      def biIter : MyBiIterImpl[A]
    }

    case class Empty[A]() extends BiIterableTree[A] {
      def biIter = new MyBiIterImpl(Nil, None, Nil)
    }

    case class Node[A](value: A, left: BiIterableTree[A], right: BiIterableTree[A])
        extends BiIterableTree[A] {
      def biIter = new MyBiIterImpl(Nil, Some(value),
        MyBiIterImpl.toList(left.biIter) ++ MyBiIterImpl.toList(right.biIter))
    }
  }

}
