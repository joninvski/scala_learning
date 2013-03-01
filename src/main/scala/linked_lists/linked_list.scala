abstract class MyLinkedList[A] {
  def head: A
  def tail: MyLinkedList[A]
  def isEmpty: Boolean
  def length: Int
  def ::(item: A): MyLinkedList[A] = new MyLinkedListImpl[A](item, this)
}

abstract class MyDoubleLinkedList[A] extends MyLinkedList[A] {
  override def ::(item: A): MyDoubleLinkedList[A] = new MyDoubleLinkedListImpl[A](item, this)
  var previous: MyDoubleLinkedList[A]
}

class MyDoubleLinkedListImpl[A](val head: A,  val tail: MyDoubleLinkedList[A]) extends MyDoubleLinkedList[A] {
  def isEmpty = false
  def length: Int = 1 + tail.length
  override def toString: String = head + " " + tail
  previous = tail
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

object MyDoubleLinkedListNil extends MyDoubleLinkedList[Nothing] {
  def head: Nothing = throw new Exception("head of empty list")
  def tail: MyDoubleLinkedList[Nothing] = throw new Exception("tail of empty list")
  def previous: MyDoubleLinkedList[Nothing] = throw new Exception("tail of empty list")
  def isEmpty = true
  def length = 0
  override def toString =  ""
}

object MyDoubleLinkedList {
  def apply[A](items: A*): MyDoubleLinkedList[A] = {
    var list: MyDoubleLinkedList[A] = MyDoubleLinkedListNil.asInstanceOf[MyDoubleLinkedList[A]]
    for (idx <- 0 until items.length reverse)
      list = items(idx) :: list
    list
  }
}

object Main extends App {
  var list = MyLinkedList("ABC", "WXYZ", "123")
  var doubleList = MyDoubleLinkedList("ola")
  // 3.14159 :: list
  Console.println(list);
}
