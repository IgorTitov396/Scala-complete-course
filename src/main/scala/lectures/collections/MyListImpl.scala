package lectures.collections

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Представим, что по какой-то причине Вам понадобилась своя обертка над списком целых чисел List[Int]
  *
  * Вы приняли решение, что будет достаточно реализовать 4 метода:
  * * * * * def flatMap(f: (Int => MyList)) -  реализуете на основе соответствующего метода из List
  * * * * * метод map(f: (Int) => Int) - с помощью только что полученного метода flatMap класса MyList
  * * * * * filter(???) - через метод flatMap класса MyList
  * * * * * foldLeft(acc: Int)(???) - через декомпозицию на head и tail
  *
  * Для того, чтобы выполнить задание:
  * * * * * раскомментируйте код
  * * * * * замените знаки вопроса на сигнатуры и тела методов
  * * * * * не используйте var и мутабильные коллекции
  *
  */
object MyListImpl extends App {

  class MyList[A, B <: Seq[_]](val data: Seq[A]) {

    def flatMap[T](f: (A => MyList[T, B])): MyList[T, B] =
      MyList(data.flatMap(f(_).data))

    def map[T](f: (A => T)): MyList[T, B] = MyList(this.data.map(f))

    def foldLeft(acc: A)(f: ((A, A)) => A): A = data match {
      case Nil => acc
      case head :: tail => MyList(tail).foldLeft(f(acc, head))(f)
    }

    def filter(f: A => Boolean): MyList[A, B] = flatMap {
      ((value: A) => if (f(value)) Seq(value) else Seq()) andThen MyList[A, B]
    }
  }

  object MyList {
    def apply[A, B <: Seq[_]](data: Seq[A]): MyList[A, B] = new MyList[A, B](data)
  }

  case class MyIndexedList[A](override val data: IndexedSeq[A]) extends MyList[A, IndexedSeq[A]](data)

  case class MyListBuffer[A](override val data: ListBuffer[A]) extends MyList[A, ListBuffer[A]](data)

  require(MyList(List(1, 2, 3, 4, 5, 6)).map(_ * 2).data == List(2, 4, 6, 8, 10, 12))
  require(MyList(List(1, 2, 3, 4, 5, 6)).filter(_ % 2 == 0).data == List(2, 4, 6))
  require(MyList(List(1, 2, 3, 4, 5, 6)).foldLeft(0)((tpl) => tpl._1 + tpl._2) == 21)
  require(MyList(Nil).foldLeft(0)((tpl) => tpl._1 + tpl._2) == 0)

  require(MyList[Int, List[Int]](List(1, 2, 3, 4, 5, 6)).map(p => p * 2).data == List(2, 4, 6, 8, 10, 12))
  require(MyList[Long, ListBuffer[Long]](ListBuffer(1, 2, 3, 4, 5, 6)).filter(_ % 2 == 0).data == List(2, 4, 6))
  require(MyList[Int, List[Int]](List(1, 2, 3, 4, 5, 6)).foldLeft(0)((tpl) => tpl._1 + tpl._2) == 21)
  require(MyList[Float, IndexedSeq[Float]](ArrayBuffer.empty[Float]).foldLeft(0)((tpl) => tpl._1 + tpl._2) == 0)

  require(MyIndexedList[Float](ArrayBuffer.empty[Float]).foldLeft(0)((tpl) => tpl._1 + tpl._2) == 0)
  require(MyListBuffer[Long](ListBuffer(1, 2, 3, 4, 5, 6)).filter(_ % 2 == 0).data == List(2, 4, 6))
}