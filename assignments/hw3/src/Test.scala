package pp201701.hw3test
import pp201701.hw3.Main._
import pp201701.hw3.Data.DataBundle._

object Test extends App {
  def print_result(b:Boolean) : Unit =
    if (b) println("O") else println("X")

  // Problem 1 - stack
  {
    val stk0 : Stack[Int] = emptyStk[Int]
    val stk1 : Stack[Int] = push[Int](stk0)(3)
    val opt_r : Option[(Int,Stack[Int])] = pop(stk1)
    print_result(
      opt_r match {
        case Some((n,_)) => n == 3
        case _ => false
      }
    )

    val list: List[Int] = (0 until 10).toList
    val myStack: Stack[Int] = pushList[Int](emptyStk[Int])(list)
    def printStack[A](stk: Stack[A]): Unit = stk match {
      case MyNil() => println("end")
      case MyCons(hd, tl) => print(hd + " "); printStack[A](tl)
    }
    printStack[Int](myStack)

    def popStack[A](stk: Stack[A]): Unit = {
      val vs = pop[A](stk)
      vs match {
        case None => println("end")
        case Some(pair) => print(pair._1 + " "); popStack[A](pair._2)
      }  
    }
    popStack[Int](myStack)
  }

  // Problem 1 - queue
  {
    val q0 : Queue[Int] = emptyQ[Int]
    val q1 : Queue[Int] = enQ(q0)(3)
    val opt_r : Option[(Int, Queue[Int])] = deQ(q1)
    print_result(
      opt_r match {
        case Some((n,_)) => n == 3
        case _ => false
      }
    )
    val list: List[Int] = (0 until 10).toList
    val myQueue: Queue[Int] = enQList[Int](emptyQ[Int])(list)
    val myQueue2: Queue[Int] = deQ[Int](deQ[Int](myQueue).get._2).get._2
    val myQueue3: Queue[Int] = enQList[Int](myQueue2)(list)
    def dequeue[A](queue: Queue[A]): Unit = {
      val vs = deQ[A](queue)
      vs match {
        case None => println("end")
        case Some(pair) => print(pair._1 + " "); dequeue[A](pair._2)
      }  
    }
    dequeue[Int](myQueue3)
  }

  // Problem 2
  {
    val cmp = (scala.math.Ordering.Int.compare _).curried
    val emptyIntIntBST : BSTree[Int, Int] = emptyBST[Int, Int]
    val insertedBST = insert(emptyIntIntBST)((1, 3))(cmp)
    val opt_r = lookup(insertedBST)(1)(cmp)
    print_result(
      opt_r match {
        case Some(n) => n == 3
        case _ => false
      }
    )

    def printTree[V](tree: BSTree[Int, V]): Unit = tree match {
      case Leaf() => 
      case Node((key, value), left, right) => {
        printTree(left)
        println(key)
        printTree(right)
      }
    }

    val keyValueList = (0 until 10).toList.reverse.zipWithIndex
    val myTree = insertList[Int, Int](emptyIntIntBST)(keyValueList)(cmp)

    printTree[Int](myTree)

    for {
      i <- 0 until 10
    } {
      print_result(
        lookup(myTree)(i)(cmp) match {
          case Some(n) => n == 9 - i
          case _ => false
        }
      )
    }

    print_result(
      lookup(myTree)(10)(cmp) match {
        case Some(n) => false
        case None => true
      }
    )
  }

  // Problem 3
  {
    val mc = new MyClass[Int, Int, Int, Int, Int, Int]()

    val obj1: mc.Ty1 = new {
      def apply: { val func: mc.Func1 ; val c: Int } => { val b: Int ; val d: Int } =
        (x) => new { val b = 0 ; val d = 1 }
      val a: Int = 1
      val b: Int = 2
    }

    val obj2: mc.Ty2 = new {
      def apply: { val func: mc.Func2 ; val e: Int } => { val b: Int ; val f: Int } =
        x => new { val b = 1 ; val f = 0 }
      val a: Int = 0
      val c: Int = 0
    }

    val r1 = mc(obj1, 0,0,0,0,0,0)
    val r2 = mc(obj2, 0,0,0,0,0,0)

    // We cannot provide test code since it gives the type information of the result,
    // that is highly related to the solution.
    println("Test problem 3 by yourself")
    print_result(r1.b == 0)
    print_result(r2.b == 1)
  }
}
