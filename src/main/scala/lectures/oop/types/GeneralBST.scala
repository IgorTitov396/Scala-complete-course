package lectures.oop.types

import scala.math.Ordering

/**
  * Модифицируйте реализацию BSTImpl из предыдущего задания.
  * Используя тайп параметры и паттерн Type Class, реализуйте GeneralBSTImpl таким образом,
  * чтобы дерево могло работать с произвольным типом данных.
  *
  * Наследников GeneralBSTImpl определять нельзя.
  *
  * Создайте генератор для деревьев 3-х типов данных:
  * * * * float
  * * * * String
  * * * * Watches из задачи SortStuff. Большими считаются часы с большей стоимостью
  */

trait GeneralBST[T] {
  val value: T
  val left: Option[GeneralBST[T]]
  val right: Option[GeneralBST[T]]

  def add(newValue: T): GeneralBST[T]

  def find(value: T): Option[GeneralBST[T]]
}


case class GeneralBSTImpl[T](value: T,
                         left: Option[GeneralBSTImpl[T]] = None,
                         right: Option[GeneralBSTImpl[T]] = None)(implicit ord: Ordering[T]) extends GeneralBST[T] {

  override def add(newValue: T): GeneralBST[T] = {
    if (ord.lt(newValue, this.value)) {
      left.fold(
        this.copy(
          left = Some(GeneralBSTImpl(newValue))
        )
      )( bst_left =>
        this.copy(
          left = Some(bst_left.add(newValue).asInstanceOf[GeneralBSTImpl[T]])
        )
      )
    }
    else {
      right.fold(
        this.copy(
          right = Some(GeneralBSTImpl(newValue))
        )
      )( bst_right =>
        this.copy(
          right = Some(bst_right.add(newValue).asInstanceOf[GeneralBSTImpl[T]])
        )
      )
    }
  }

  override def find(value: T): Option[GeneralBST[T]] = {
    if (ord.equiv(value, this.value)) Some(this)
    else if (ord.gt(this.value, value)) left.flatMap(_.find(value))
    else right.flatMap(_.find(value))
  }
}

object FloatTreeTest extends App {

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount = sc.nextInt()

  val markerItem = (Math.random() * maxValue).toFloat
  val markerItem2 = (Math.random() * maxValue).toFloat
  val markerItem3 = (Math.random() * maxValue).toFloat

  // Generate huge tree
  def floatGeneratror(bstTree: GeneralBST[Float], acc: Int = 0): GeneralBST[Float] = acc match {
    case value if value == nodesCount - 1 => bstTree
    case _ => floatGeneratror(bstTree.add((Math.random() * maxValue).toFloat), acc + 1)
  }

  val root = GeneralBSTImpl[Float](maxValue / 2)
  val tree = floatGeneratror(root)

  // add marker items
  val testTree = tree.add(markerItem).add(markerItem2).add(markerItem3)

  // check that search is correct
  require(testTree.find(markerItem).map(_.value).contains(markerItem))
  require(testTree.find(markerItem2).map(_.value).contains(markerItem2))
  require(testTree.find(markerItem3).map(_.value).contains(markerItem3))
}

object StringTreeTest extends App {

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount = sc.nextInt()

  val markerItem = (Math.random() * maxValue).toInt.toString
  val markerItem2 = (Math.random() * maxValue).toInt.toString
  val markerItem3 = (Math.random() * maxValue).toInt.toString

  // Generate huge tree
  def floatGeneratror(bstTree: GeneralBST[String], acc: Int = 0): GeneralBST[String] = acc match {
    case value if value == nodesCount - 1 => bstTree
    case _ => floatGeneratror(bstTree.add((Math.random() * maxValue).toInt.toString), acc + 1)
  }

  val root = GeneralBSTImpl[String]((maxValue / 2).toString)
  val tree = floatGeneratror(root)

  // add marker items
  val testTree = tree.add(markerItem).add(markerItem2).add(markerItem3)

  // check that search is correct
  require(testTree.find(markerItem).map(_.value).contains(markerItem))
  require(testTree.find(markerItem2).map(_.value).contains(markerItem2))
  require(testTree.find(markerItem3).map(_.value).contains(markerItem3))
}
