abstract class MyLinkedList[A] {
  def head: A
  def tail: MyLinkedList[A]
  def isEmpty: Boolean
  def length: Int
  def ::(item: A): MyLinkedList[A] = new MyLinkedListImpl[A](item, this)
}

abstract class MyDoubleLinkedList[A] extends MyLinkedList[A] {
  override def ::(item: A): MyDoubleLinkedList[A] = new MyDoubleLinkedListImpl[A](item, this)
}

class MyDoubleLinkedListImpl[A](val head: A, val tail: MyDoubleLinkedList[A]) extends MyDoubleLinkedList[A] {
  def isEmpty = false
  def length: Int = 1 + tail.length
  override def toString: String = head + " " + tail
}

class MyLinkedListImpl[A](val head: A, val tail: MyLinkedList[A]) extends MyLinkedList[A] {
  def isEmpty = false
  def length: Int = 1 + tail.length
  override def toString: String = head + " " + tail
}

object MyLinkedListNil extends MyLinkedList[Nothing] {
  def head: Nothing = throw new Exception("head of empty list")
  def tail: MyLinkedList[Nothing] = throw new Exception("tail of empty list")
  def isEmpty = true
  def length = 0
  override def toString =  ""
}

object MyLinkedList {
  def apply[A](items: A*): MyLinkedList[A] = {
    var list: MyLinkedList[A] = MyLinkedListNil.asInstanceOf[MyLinkedList[A]]
    for (idx <- 0 until items.length reverse)
      list = items(idx) :: list
    list
  }
}

object Main extends App {
  var list = MyLinkedList("ABC", "WXYZ", "123")
  var doubleList = MyDoubleLinkedList("ABC", "WXYZ", "123")
  // 3.14159 :: list
  Console.println(list);
}
