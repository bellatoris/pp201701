package pp201701.hw4test
import pp201701.hw4.Main._
import pp201701.hw4.Data.DataBundle._
import pp201701.hw4.Main.Problem2._

object Test extends App {
  def print_result(b:Boolean) : Unit =
    if (b) println("O") else println("X")

  // Problem 1
  {
    val intEq: Int => Int => Boolean = { x => y => (x == y) }
    val d0 = Problem1.IterDictImpl.empty[Int, Int](intEq)
    val d1 = d0.add(1, 2)

    print_result(
      d1.find(1) match {
        case Some(n) => n == 2
        case _ => false
      }
    )
  }

  // Problem 2
  {
    val x = List("A", "B", "C")
    val bx = new Problem2.BiIterableList(x)

    print_result(
      bx.biIter.getValue match {
        case Some(x) => x == "A"
        case _ => false
      }
    )

    println(bx.biIter.getPrev.getValue)  
    println(bx.biIter.getPrev.getPrev.getValue)  
    println(bx.biIter.getPrev.getPrev.getPrev.getPrev.getNext.getValue)  
    println(bx.biIter.getNext.getNext.getNext.getNext.getValue)
    println(bx.biIter.getNext.getNext.getNext.getNext.getNext.getValue)
    println(bx.biIter.getNext.getNext.getNext.getNext.getNext.getNext.getPrev.getValue)

    val t : Problem2.BiIterableTree[Int] =
        Node(3, Node(4,Node(2,Empty(),Empty()),
            Node(3,Empty(),Empty())),
              Node(5,Empty(),Empty()))
    println(t.biIter.getValue)
    println(t.biIter.getNext.getValue)
    println(t.biIter.getNext.getNext.getValue)
    println(t.biIter.getNext.getNext.getNext.getValue)
    println(t.biIter.getNext.getNext.getNext.getNext.getValue)
    println(t.biIter.getNext.getNext.getNext.getNext.getNext.getValue)
    println(t.biIter.getNext.getNext.getNext.getNext.getNext.getPrev.getValue)
    println(t.biIter.getNext.getNext.getNext.getNext.getNext.getPrev.getPrev.getValue)
    println(t.biIter.getNext.getNext.getNext.getNext.getNext.getPrev.getPrev.getPrev.getValue)
   }
}
