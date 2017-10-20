package lectures.oop

import scala.util.Random


/**
  * BSTImpl - это бинарное дерево поиска, содержащее только значения типа Int
  *
  * * Оно обладает следующими свойствами:
  * * * * * левое поддерево содержит значения, меньшие значения родителя
  * * * * * правое поддерево содержит значения, большие значения родителя
  * * * * * значения, уже присутствующие в дереве, в него не добавляются
  * * * * * пустые значения (null) не допускаются
  *
  * * Завершите реализацию методов кейс класс BSTImpl:
  * * * * * Трейт BST и BSTImpl разрешается расширять любым образом
  * * * * * Изменять сигнатуры классов и методов, данные в условии, нельзя
  * * * * * Постарайтесь не использовать var и мутабильные коллекции
  * * * * * В задаче про распечатку дерева, нужно раскомментировать и реализовать метод toString()
  *
  * * Для этой структуры нужно реализовать генератор узлов.
  * * Генератор:
  * * * * * должен создавать дерево, содержащее nodesCount узлов.
  * * * * * не должен использовать переменные или мутабильные структуры.
  *
  */
trait BST {
  val value: Int
  val left: Option[BST]
  val right: Option[BST]

  def add(newValue: Int): BST

  def find(value: Int): Option[BST]

  def myToString():String
}

case class BSTImpl(value: Int,
                   left: Option[BSTImpl] = None,
                   right: Option[BSTImpl] = None) extends BST {

  def add(newValue: Int): BST = {
    if (newValue < value) {
      if (left.isDefined) this.copy(left = Option(left.get.add(newValue).asInstanceOf[BSTImpl])) else this.copy(left = Option(BSTImpl(newValue)))
    }
    else {
      if (right.isDefined) this.copy(right = Option(right.get.add(newValue).asInstanceOf[BSTImpl])) else this.copy(right = Option(BSTImpl(newValue)))
    }
  }

  def find(value: Int): Option[BST] = {
    if (value == this.value) Option(this)
    else if (value > this.value) right
    else left
  }

  override def myToString() = {
    def depthOfBST(acc: Int, bst: BST): Int = bst match {
      case BSTImpl(_, left, right) if left.isDefined && right.isDefined => math.max(depthOfBST(acc + 1, left.get), depthOfBST(acc + 1, right.get))
      case BSTImpl(_, left, _) if left.isDefined => depthOfBST(acc + 1, left.get)
      case BSTImpl(_, _, right) if right.isDefined =>  depthOfBST(acc + 1, right.get)
      case _ => acc
    }
    depthOfBST(1, this).toString
  }
  //override def toString()
}

object TreeTest extends App {

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount = sc.nextInt()

  val markerItem = (Math.random() * maxValue).toInt
  val markerItem2 = (Math.random() * maxValue).toInt
  val markerItem3 = (Math.random() * maxValue).toInt

  // Generate huge tree
  def generatror(acc: Int, bstTree: BST): BST = acc match {
    case value if value == nodesCount => bstTree.add(Random.nextInt(maxValue))
    case _ => generatror(acc + 1, bstTree.add(Random.nextInt(maxValue)))
  }

  val root: BST = BSTImpl(maxValue / 2)
  val tree: BST = generatror(0, root)

  println(tree.myToString())
  println(tree)
  // add marker items
  val testTree = tree.add(markerItem).add(markerItem2).add(markerItem3)

  // check that search is correct
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem).isDefined)

  println(testTree)
}