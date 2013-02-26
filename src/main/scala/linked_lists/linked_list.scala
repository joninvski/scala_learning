abstract class MyList[A] {
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def length: Int
  def ::(item: A): MyList[A] = new MyListImpl[A](item, this)
}

class MyListImpl[A](val head: A, val tail: MyList[A]) extends MyList[A] {
  def isEmpty = false
  def length: Int = 1 + tail.length
  override def toString: String = head + " " + tail
}

object MyListNil extends MyList[Nothing] {
  def head: Nothing = throw new Exception("head of empty list")
  def tail: MyList[Nothing] = throw new Exception("tail of empty list")
  def isEmpty = true
  def length = 0
  override def toString =  ""
}

object MyList {
  def apply[A](items: A*): MyList[A] = {
    var list: MyList[A] = MyListNil.asInstanceOf[MyList[A]]
    for (idx <- 0 until items.length reverse)
      list = items(idx) :: list
    list
  }
}

object Main extends App {
    Console.println("Hello, world!");
}
