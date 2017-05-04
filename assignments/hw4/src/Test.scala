package pp201701.hw4test
import pp201701.hw4.Main._
import pp201701.hw4.Data.DataBundle._
import pp201701.hw4.Main.Problem2._
import pp201701.hw4.Main.Problem1._

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
    println(d1.find(1))
    println(sumElements(d1))
    
    val d2 = d1.add(1, 3)
    println(d2.find(1))
    println(sumElements(d2))

    val d3 = d2.add(2, 4).add(3, 6).add(4, 8).add(5, 10)
    println(sumElements(d3))
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
    
    val t : Problem2.BiIterableTree[Int] =
        Node(3, Node(4,Node(2,Empty(),Empty()),
            Node(3,Empty(),Empty())),
              Node(5,Empty(),Empty()))

    def nthNext(t: BiIter[Int], n: Int): BiIter[Int] = n match {
      case 0 => t
      case _ => nthNext(t.getNext, n - 1)
    }
    def nthPrev(t: BiIter[Int], n: Int): BiIter[Int] = n match {
      case 0 => t
      case _ => nthPrev(t.getPrev, n - 1)
    }

    for (i <- 0 until 10) {
      println(nthNext(t.biIter, i).getValue)
    }
    for (i <- 0 until 10) {
      println(nthPrev(nthNext(t.biIter, 10), i).getValue)
    }
  }
}
